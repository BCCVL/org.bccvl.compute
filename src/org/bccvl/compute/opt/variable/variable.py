#!/usr/bin/env python

class subset(object):
    def __init__(self, json_dict, params={}):
        self.params={"min": 1, "max": -1, "json_dict_path": None} # defaults
        self.params.update(params) # user supplied

        self.set=reduce(lambda d, k: d[k], self.params["json_dict_path"], json_dict)
        if (self.params["max"] == -1): self.params["max"] = len(self.set) #-1 max means "auto set"

    def random_choice(self):
        import random
        n = random.randint(self.params["min"], self.params["max"])
        selected = random.sample(self.set, n)
        return selected

    def every_choice(self):
        # could be potentially huge
        import itertools
        possible_combinations = []
        for i in range(self.params["min"], self.params["max"]+1):
            possible_combinations.extend([value for value in itertools.combinations(self.set, i)])
        return possible_combinations 

    def parameter(self, k):
        return self.params[k]
   
    # binary encoding
    def extract(self, json_dict):
        if (len(self.set) > 63): 
            raise Exception("Set is too large to encode as binary int")
        value=reduce(lambda d, k: d[k], self.params["json_dict_path"], json_dict)
        return int("".join([ "1"if v in value else "0" for v in self.set ]), 2) #binary to int
 
class integer_range(object):
    def __init__(self, json_dict, params={}):
        self.params={ "min": None, "max": None, "json_dict_path": None, "step":1} # defaults
        self.params.update(params) # user supplied

    def random_choice(self):
        import random
        n = random.randrange(self.params["min"], self.params["max"]+1, self.params["step"])
        return n
    
    def every_choice(self):
        return range(self.params["min"], self.params["max"]+1, self.params["step"])

    def parameter(self, k):
        return self.params[k]

    def extract(self, json_dict, encode=True):
        return reduce(lambda d, k: d[k], self.params["json_dict_path"], json_dict)

class double_range(object):
    def __init__(self, json_dict, params={}):
        self.params = { "min": None, "max": None, "json_dict_path": None, "n":None } # default
        self.params.update(params) # user supplied
        if not self.params["n"] or self.params["n"] <= 1:
            raise Exception("n parameter must supplied and greater than 1") 

    def random_choice(self):
        import random
        #if self.params["n"] == None:
        #    d = random.random()*(self.params["max"] - self.params["min"]) + self.params["min"]
        
        n = random.randrange(0, self.params["n"])
        d = self.params["min"] + n*(self.params["max"] - self.params["min"])/(self.params["n"]-1.0)
            
        return d 

    def every_choice(self):
        return [self.params["min"] + x *(self.params["max"] - self.params["min"])/(self.params["n"]-1.0) for x in range(0, self.params["n"])]

    def parameter(self, k):
        return self.params[k]

    def extract(self, json_dict, encode=True):
        return reduce(lambda d, k: d[k], self.params["json_dict_path"], json_dict)

def variable_factory_get(json_dict, params):
    if params["var_type"] == "integer_range": return integer_range(json_dict, params)
    if params["var_type"] == "double_range":  return double_range(json_dict, params)
    if params["var_type"] == "subset":        return subset(json_dict, params)

    raise Exception("Unhandled var type: " + params["var_type"] + " (in variable_factory_get)") 
