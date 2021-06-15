#!/bin/bash

R -e 'shiny::runApp("/root/clusterProfiler_GOenrich",port=2525,host="0.0.0.0")'

