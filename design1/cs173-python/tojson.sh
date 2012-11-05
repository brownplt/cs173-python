#!/bin/bash

cat $1 | /course/cs173/python/Python-3.2.3/python python-parser.py | /course/cs173/python/Python-3.2.3/python bin/jsbeautifier.py -i
