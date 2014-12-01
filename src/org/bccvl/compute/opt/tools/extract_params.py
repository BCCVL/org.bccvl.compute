#!/usr/bin/env python

import sys
import os
sys.path.append(os.path.dirname(os.path.realpath(__file__))+"/../variable")
import variable

import json

def main():
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("--input", dest="input",
                      help="the input json file", metavar="FILE")
    parser.add_option("--search-variables", dest="search_variables",
                      help="A text file describing searchable variables and their corresponding ranges")
    parser.add_option("--output", dest="output", default=None,
                      help="output csv")

    (options, args) = parser.parse_args()

    json_dict=json.loads(open(options.input).read())
    variable_entries = [ variable.variable_factory_get(json_dict, eval(line)) 
                         for line in open(options.search_variables) ]
    
    of = sys.stdout if options.output == None else open(options.output, "w")

    outputs=[str(v.extract(json_dict)) for v in variable_entries]

    of.write(",".join(outputs))
    of.close()

if __name__ == '__main__':
    main()

