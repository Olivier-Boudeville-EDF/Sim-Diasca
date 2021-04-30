import sys

""" this class hooks stdout and stderr streams and redirects them to aself.func trace emitter 
as per convention stdout is traced as debug info, stderr as errors """

class IOHook:
    def __init__(self,trace_emitter):
        self.origOut = None
        self.origErr = None
        self.emitter = trace_emitter

    def start(self):
        self.origOut = sys.stdout
        self.origErr = sys.stderr
        sys.stdout = TraceWriter(self.emitter, self.origOut, False)
        sys.stderr = TraceWriter(self.emitter, self.origErr, True)

    #Stop will stop routing of print statements thru this class
    def stop(self):
        if self.origOut != None:
            sys.stdout = self.origOut
        if self.origErr != None:
            sys.stdout = self.origErr

"""
    Simulate a python file object but writes to trace_emitter passed at construction.
    if is_error is set to true it is traced as error, if not it is trace
"""
class TraceWriter:

    def __init__(self, trace_emitter, origstream, is_error):
        self.emitter = trace_emitter
        self.error = is_error
        self.origstream = origstream

    #override write of stdout
    def write(self,text):
        text = text.strip()
        if not text: return
        if self.error:
            self.emitter.send_error('STDERR: '+text)
        else:
            self.emitter.send_trace('STDOUT: '+text)


    #pass all other methods to __stdout__ so that we don't have to override them
    def __getattr__(self, name):
        return getattr(self.origstream,name)