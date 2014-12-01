#!/usr/bin/env python

import json

def main():
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("--input", dest="input",
                      help="input json file")
    parser.add_option("--output", dest="output",
                      help="output json file")
    parser.add_option("--attr-value", dest="attr_value",action="append", default=[],
                      help="Json attribute value pairs formatted like \"params/algo/value1=alpha\". "\
                      "Note the \"/\" is the means for specifying nested attributes.")
    (options, args) = parser.parse_args()

    json_dict=json.loads(open(options.input).read())
    
    for attr_value in options.attr_value:
        [keys, value] = attr_value.split("=")
        keys = keys.split("/")
        entry=reduce(lambda d, k: d[k], keys[:-1], json_dict)
        entry[keys[-1]] = value 

    of = open(options.output, "w")
    of.write(json.dumps(json_dict, indent=1)+"\n")
    of.close()

if __name__ == '__main__':
    main()

