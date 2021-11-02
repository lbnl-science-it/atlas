#!/usr/bin/env Rscript

print("Hello Word from R!")

# run this example as
# ./eg_option_parser.R -i some/path --outdir /some/other/absolute/path

suppressPackageStartupMessages(library("optparse"))

# optparse
# https://cran.r-project.org/web/packages/optparse/vignettes/optparse.html
# argparse maybe easier, but has a dependency in python and I rather avoid that since we don't need complex stuff here

# see also
# https://stackoverflow.com/questions/3433603/parsing-command-line-arguments-in-r-scripts

option_list <- list( 
    make_option(c("-i", "--indir"),  dest="inputdir",  action="store", help="path to input  data", default="tbd" ),
    make_option(c("-o", "--outdir"), dest="outputdir", action="store", help="path to output data", default="tbd" )
    )

opt <- parse_args(OptionParser(option_list=option_list))


print( "input  directory specified as:") 
print( opt$inputdir )
print( "--" )
print( "output directory specified as:")
print( opt$outputdir )


