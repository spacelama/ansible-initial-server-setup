#!/usr/bin/python
import sys

class FilterModule(object):
    def filters(self):
        return {
            'bitwise_and_oct': self.bitwise_and_oct
        }

    def bitwise_and_oct(self, a, b):
        c = oct(int(a,8) & int(b,8))
#        print("a=", int(a,8), "b=", int(b,8), "c=", c, file=sys.stderr)
        return c
