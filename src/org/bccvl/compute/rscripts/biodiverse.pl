#!/usr/bin/perl -w
use strict;
use warnings;
use 5.010;

use rlib;

use Biodiverse::Config ;

use Carp;
use English qw { -no_match_vars };

use JSON ;
use File::Basename ;
use File::Spec ;

use Text::CSV_XS;

use Biodiverse::BaseData;
use Biodiverse::ElementProperties;

use Geo::GDAL;

use List::Util qw[min max];

# TODO: do proper error handling on reading json file and checking for available parameters
#####################################
# read parameters
open( my $fp, '<', 'params.json' );
my $json_text ;
{
    local $/;
    $json_text   = <$fp>;
}
my %bccvl_params = %{ decode_json($json_text) };
my %bccvl_env = %{ $bccvl_params{'env'} };
%bccvl_params = %{ $bccvl_params{'params'} };

####################################
# change current dir to output folder
my $outdir = $bccvl_env{'outputdir'};
chdir($outdir);

#####################################
# subroutine to reproject a given gdal file and write the
# result to given output folder
# return full path of new file
# TODO: make sure we generate unique filenames.... maybe use full path within input to generate output?
sub gdalwarp {
    my ($infile, $outdir, $srs) = @_;
    my ($name, $path, $suffix) = fileparse($infile, qr/\.[^.]*/);
    my $destfile = File::Spec->catfile($outdir, $name . ".tif");

    # build argumentlist for gdalwarp
    my @warp_args = ("gdalwarp");
    # check if source file has SRS assigned ... if not use default EPSG:4326 (WGS84)
    my $dataset = Geo::GDAL::Open($infile, 'ReadOnly');
    my $spref = $dataset->GetProjectionRef();
    if (! $spref) {
        say "Force source srs to EPSG:4326";
        push(@warp_args, ('-s_srs', 'EPSG:4326'));
    }

    # GDAL befor 1.11 has no automatic nodata handling in gdal warp ...
    # for these versions we probably should read nodata from orig dataset
    # and set it as '-dstnodata' value here....
    # TODO: remove once we have GDAL 1.11 + everywhere
    my $nodata = $dataset->Band(1)->NoDataValue();
    if (defined($nodata)) {
        # we have no data in source .. pass it on
        push(@warp_args, ('-dstnodata', $nodata));
    }
    #  nothing to do in case no data is not defined

    push(@warp_args, ('-t_srs', $srs, $infile, $destfile));
    say "try to transform $_->{'filename'} to $destfile";
    system(@warp_args);
    # TODO: $! .... knows whether it all went well
    say "gdalwarp:", $! ;
    say $destfile;
    return $destfile;
}

######################################
# subroutine to set all values below threshold in
# a rasterfile to 0 and others to 1. Leave Nodata as is.
# -> essentially a binary transformation
# subroutine assumes there is only one band in the file
sub apply_threshold {
    # TODO: should this create a BinaryMap ? 0 for below threshold and 1 for above?
    my $errmsg = '';
    my ($filename, $threshold) = @_;
    say "APPLY: $filename, $threshold";
    # Open file for update
    my $dataset = Geo::GDAL::Open($filename, 'Update');

    my $band = $dataset->Band(1);
    my ($blockw, $blockh) = $band->GetBlockSize();
    my ($bandw, $bandh) = $band->Size();
    # read vertical blocks until max height reached
    my $posh = 0 ;
    my $nodata = $band->NoDataValue();

    # GDAL may not return a valid nodata value. Either because of a bug in
    # the GDAL library or there is no nodata value defined.
    # Either way, we make sure we have a defined no data value
    my $nodatadefined = defined($nodata);
    if (!$nodatadefined) {
        say "NoData not defined using default -9999";
        $nodata = -9999;
        $band->NoDataValue($nodata);
    } else {
        say "Use NoData $nodata";
    }
    my $allsame = undef;  # flag to see whether all raster values are 0 or 1 after thresholding

    while ($posh < $bandh) {
        # read horizontal blocks until max width reached
        my $posw = 0 ;
        while ($posw < $bandw) {
            # determine max block size to read
            my $maxw = min($bandw, $posw + $blockw);
            my $maxh = min($bandh, $posh + $blockh);
            # read block (a perl array $block->[x][y] 0..max-1)
            my $block = $band->ReadTile($posw, $posh, $maxw - $posw, $maxh - $posh);
            # iterate over each element in the block with indices so that
            # we can update in place
            for(my $row_i = 0; $row_i < @{$block}; $row_i++) {
                my $row = $block->[$row_i];
                for(my $col_i = 0; $col_i < @{$row}; $col_i++) {
                    my $val = $row->[$col_i];
                    if ((!$nodatadefined) && ($val < 0)) {
                        # we have no idea what nodata is,
                        # assume that anything smaller than 0 is nodata
                        # we are dealing with species distribution probabilities
                        # which should not have negative values in any case
                        $row->[$col_i] = $nodata;
                        next;
                    }
                    if ($nodatadefined && (abs($val - $nodata) < 1E-10)) {
                        # we have a value very close to nodatavalue,
                        # set to $nodata
                        $row->[$col_i] = $nodata;
                        next;
                    }

                    # BCCVL-103: Change value below threshold to $nodata
                    # This should be done here.
                    my $newval = $val >= $threshold ? $val : $nodata;

                    $row->[$col_i] = $newval;

                    if (!defined($allsame)) {
                        $allsame = $newval ;
                    } else {
                        if ($allsame >= 0 && $allsame != $newval) {
                            $allsame = -1 ;
                        }
                    }

                }
            }
            # write block back to file
            $band->WriteTile($block, $posw, $posh);
            $posw += $blockw;
        }
        $posh += $blockh;
    }
    # recalc statistics and set as metadat
    my ($min, $max, $mean, $stddev) = $band->ComputeStatistics(0);
    $band->SetStatistics($min, $max, $mean, $stddev);
    # Flush and close file
    $dataset->FlushCache();
    if (!defined($allsame)) {
        $errmsg = "Nothing but NoData ($nodata) found in in put dataset.";
    }
    if (defined($allsame) && $allsame >= 0) {
        # we have a problem here. after thresholding all data values are the same
        # (0 or 1). We can't run the algorithm, but we can give the user meaningful
        # message
        $errmsg = "Thresholding (with '$threshold') turned every single value in the raster to $allsame.";
    }
    return $errmsg;
}

##########################################
# Read from a group csv file and return a dictionary of element to list of species.
sub get_species
{
    my %species;
    my $filename = shift;

    # open the file for reading
    my $csv = Text::CSV_XS->new ({ binary => 1, auto_diag => 1, eol => $/ });
    open(my $ifh, "<", $filename) or
        die("Could not open $filename file. $!\n");

    # skip the header
    my $line = $csv->getline($ifh);

    while($line = $csv->getline($ifh)) {
        my @fields =  @$line;

        # First column is the element, 4th column is the species.
        # Skip if the 5th column (i.e. occurrence count) is 0 or undefined
        if (defined($fields[4]) && $fields[4] ne "" && $fields[4] > 0) {
            # Strip the enclosing apostrophes if any from element, as
            # this can occur with multiple species.
            my $element = $fields[0];
            $element =~ s/'//g;
            if (!exists($species{$element})) {
                $species{$element} = [$fields[3]];
            }
            else {
                push @{$species{$element}}, $fields[3];
            }
        }
    }
    close $ifh;
    return %species;
}

##########################################
# project input files and apply threshold
my %species;
my @files = () ;
foreach (@{$bccvl_params{'projections'}}) {
    my $destfile = gdalwarp($_->{'filename'}, $outdir, "epsg:3577");
    my $errmsg = apply_threshold($destfile, $_->{'threshold'}->{'value'});

    # Add only these have threshold applied successsfully.
    if (!$errmsg) {
        push(@files, $destfile);

        # save the filename and its corresponding species name
        $species{basename($destfile)} = $_->{'species'};
    }
    else {
        # Exlude the species
        say "Excluding species $_->{'species'}: $errmsg";

        # Delete the file
        unlink $destfile;
    }
}

###################################
# extract script params to orig script variables

my $cellsize = $bccvl_params{'cluster_size'};

# rest of orig script variables
# TODO: set this to something more meaningful from params.json
my $out_pfx = 'biodiverse_prefix';


##########################################
# ????

my $bd = eval {Biodiverse::BaseData->new(
    CELL_SIZES => [$cellsize, $cellsize],
)};


#####################################
#  import the data

my $success = eval {
    $bd->import_data_raster (
        input_files     => \@files,
        labels_as_bands => 1,  #  use the bands (files) as the label (species) names
    );
};
croak $EVAL_ERROR if $EVAL_ERROR;

# rename the label to species name
my $remap = Biodiverse::ElementProperties->new ();
while (my ($fname, $speciesName) = each %species) {
    $remap->add_element (element => $fname);
    my $remap_hash = {REMAP => $speciesName};
    $remap->add_lists (element => $fname, PROPERTIES => $remap_hash);
}
$bd->rename_labels (remap => $remap);

#####################################
#  analyse the data

#  need to build a spatial index if we do anything more complex than single cell analyses
#  $bd->build_spatial_index (resolutions => [$bd->get_cell_sizes]);

#  add to as needed, possibly using args later on
my $calculations = [qw/
    calc_endemism_whole
    calc_rarity_whole
/];

my $sp = $bd->add_spatial_output (name => 'spatial_analysis');
$success = eval {
        $sp->run_analysis (
        calculations       => $calculations,
        spatial_conditions => ['sp_self_only()'],
    );
};
croak $EVAL_ERROR if $EVAL_ERROR;


#  adds the .bds extension by default
$bd->save (filename => $out_pfx);


#  export the files
$sp->export (
    format => 'ArcInfo asciigrid files',
   # format => 'GTIFF',
    file   => $out_pfx,
    list   => 'SPATIAL_RESULTS',
);

# export data as csv as well
my $spatial_result_file = 'biodiverse_SPATIAL_RESULTS.csv';
$sp->export (
    format => 'Delimited text',
    file   => $spatial_result_file,
    list   => 'SPATIAL_RESULTS',
);

# This output file maps element to input file (i.e. species)
my $grp_csv_file = sprintf "%s_%s.csv", 'biodiverse', "groups";
$bd->get_groups_ref->export (
    file   =>  $grp_csv_file,
    format => 'Delimited text',
    list => 'SUBELEMENTS',
    one_value_per_line => 1,
);

# Extract the mapping of element/species from the group csv file.
# Insert the species info into the "spatial_result" file to generate
# a new csv file.
my %elem_species = get_species($grp_csv_file);

my $csv = Text::CSV_XS->new ({ binary => 1, auto_diag => 1, eol => $/ });

# The original input csv file.
open(my $input_fh, "<", $spatial_result_file) or
    die("Could not open $spatial_result_file file. $!\n");

# This is the new csv file with species column
open (my $output_fh, '>', 'biodiverse_spatial_results_merged.csv') or
    die("Could not open biodiverse_spatial_results_merged.csv file for writing");

# Add extra column for new output csv file.
my $line = $csv->getline($input_fh);
push $line, "SPECIES";
$csv->print($output_fh, $line);

# The 1st column is the element.
while ($line = $csv->getline($input_fh)) {
    my @cols = @$line;
    my $spNames = '';
    if (exists($elem_species{$cols[0]})) {
        $spNames = join(',', @{ $elem_species{$cols[0]} });
    }
    push $line, $spNames;
    $csv->print($output_fh, $line);
}
close $input_fh;
close $output_fh;

# Remove the original files
unlink $spatial_result_file;
unlink $grp_csv_file;

my $ascglob = File::Spec->catfile($outdir, "*.asc");
foreach(glob($ascglob)) {
    my ($name, $path, $suffix) = fileparse($_, qr/\.[^.]*/);
    my $destfile = File::Spec->catfile($path, $name . ".tif");
    system("gdal_translate", "-a_srs", "epsg:3577", "-stats", $_, $destfile);
    # TODO: check $! before moving on
    unlink($_);
}

say 'Completed';
