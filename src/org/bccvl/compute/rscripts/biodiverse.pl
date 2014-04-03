#!/usr/bin/perl -w
use strict;
use warnings;
use 5.010;

use rlib;

use Getopt::Long::Descriptive;

use Carp;
use English qw { -no_match_vars };

use JSON ;

use Biodiverse::BaseData;
use Biodiverse::ElementProperties;

# TODO: do proper error handling on reading json file and checking for available parameters
#####################################
# read parameters
open( my $fp, '<', 'params.json' );
my $json_text ;
{
    local $/;
    $json_text   = <$fp>;
}
my %bccvl_params = %{decode_json($json_text)};

####################################
# change current dir to output folder
my $outdir = $bccvl_params{'outputdir'};
chdir($outdir);

###################################3
# extract script params to orig script variables

my $cellsize = $bccvl_params{'clustersize'};

my @files = @{$bccvl_params{'specieslayers'}};

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

say 'Completed';
