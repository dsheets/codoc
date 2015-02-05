#!/bin/sh

sed -e s/\"share\"/\"$1\"/ cli/codocConfig.ml.in > cli/codocConfig.ml
