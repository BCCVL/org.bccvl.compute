#!/usr/bin/env python

import sys
import os
sys.path.append(os.path.dirname(os.path.realpath(__file__))+"/../variable")
import variable

import json

def next_outfile_name(index, seed_filename):
    import re
    f=os.path.basename(seed_filename)
    dot_json_file = len(re.findall(".json$", f)) == 1
    if dot_json_file:
        return re.sub(".json$", "." + str(index) + ".json", f)
    else:
        return re.sub("$",  "." + str(index), f)

def main():
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("-f", "--file", dest="filename",
                      help="the base json file", metavar="FILE")
    parser.add_option("--search-variables", dest="search_variables",
                      help="A text file describing searchable variables and their corresponding ranges")
    parser.add_option("--strategy", dest="strategy", default="grid",
                      help="Stategy string, e.g. grid, random")
    parser.add_option("--workdir", dest="workdir", default="/tmp", 
                      help="Where output files will be placed")
    parser.add_option("--stochastic-population", dest="stochastic_population",
                      help="Stochastic population. Only applicable if random is the chosen strategy")


    (options, args) = parser.parse_args()

    json_dict=json.loads(open(options.filename, "r").read())
    variable_entries = [ variable.variable_factory_get(json_dict, eval(line)) 
                         for line in open(options.search_variables) ]
   
    json_dict_path = [v.parameter("json_dict_path") for v in variable_entries]    

    # the outdir should exist
    if not os.path.exists(options.workdir): os.makedirs(options.workdir)

    if options.strategy == "grid":
        # get everything
        import itertools
        space = [ v.every_choice() for v in variable_entries ]
        out_idx=1
        for l in itertools.product(*space) :
            for index, payload in enumerate(l):
                entry=reduce(lambda d, k: d[k], json_dict_path[index][:-1], json_dict)
                entry[json_dict_path[index][-1]] = payload 
           
            next_file = next_outfile_name(out_idx, options.filename)
            next_file = os.path.join(options.workdir, next_file)
            of = open(next_file, "w")
            of.write(json.dumps(json_dict, indent=1)+"\n")
            of.close()
            out_idx += 1
    else:
        raise Exception("Only grid is currently supported")


if __name__ == '__main__':
    main()

