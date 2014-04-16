#!/usr/bin/perl -w
use strict;
use warnings;
use 5.010;

use rlib;

use Getopt::Long::Descriptive;

use Carp;
use English qw { -no_match_vars };

use JSON ;
use File::Basename ;
use File::Spec ;

use Biodiverse::BaseData;
use Biodiverse::ElementProperties;

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
sub gdalwarp {
    my ($infile, $outdir, $srs) = @_;
    my ($name, $path, $suffix) = fileparse($infile, qr/\.[^.]*/);
    my $destfile = File::Spec->catfile($outdir, $name . ".tif");
    print "try to transform $_->{'filename'} to $destfile\n";
    system("gdalwarp", "-t_srs", $srs, $infile, $destfile);
    # TODO: $! .... knows whether it all went well
    print "gdalwarp:", $!, "\n" ;
    print $destfile, "\n" ;
    return $destfile;
}

######################################
# subroutine to set all values below threshold in
# a rasterfile to No_Value
# subroutine assumes there is only one band in the file
sub apply_threshold {
    # TODO: should this create a BinaryMap ? 0 for below threshold and 1 for above?
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
                    if (defined($nodata) && $val == $nodata) {
                        next;
                    }
                    if ($val < $threshold ) {
                        $row->[$col_i] = 0 ;
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
    $dataset->FlushCache()
}

##########################################
# project input files and apply threshold
my @files = () ;
foreach (@{$bccvl_params{'projections'}}) {
    my $destfile = gdalwarp($_->{'filename'}, $outdir, "epsg:3577");
    apply_threshold($destfile, $_->{'threshold'});
    push(@files, $destfile);
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

#####################################
#  analyse the data

#  need to build a spatial index if we do anything more complex than single cell analyses
#  $bd->build_spatial_index (resolutions => [$bd->get_cell_sizes]);

#  add to as needed, possibly using args later on
my $calculations = [qw/
    calc_endemism_whole
    calc_redundancy
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
    file   => $out_pfx,
    list   => 'SPATIAL_RESULTS',
);

my $ascglob = File::Spec->catfile($outdir, "*.asc");
foreach(glob($ascglob)) {
    my ($name, $path, $suffix) = fileparse($_, qr/\.[^.]*/);
    my $destfile = File::Spec->catfile($path, $name . ".tif");
    system("gdal_translate", "-a_srs", "epsg:3577", "-stats", $_, $destfile);
    # TODO: check $! before moving on
    unlink($_);
}

say 'Completed';
