from zope import interface
class IComputeFunction(interface.Interface):

    parameters = interface.Attribute("""schema object for the user configurable parameters""")
        
    def execute(experiment):
        """
        This function takes an experiment and executes.
    
        It uses envirnoment variables WORKER_DIR or HOME as root folder to execute
        experiments.
    
        After the execution finishes the output files will be attached to the
        experiment.
    
        :param experiment: The experiment holding the configuration and receiving
                           the results
        :type experiment: org.bccvl.site.content.IExperiment
        """
        
