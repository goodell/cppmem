#!/bin/bash
    $1 -Tpdf $2 -o $3  2>&1 | grep -v "size too small for label" | grep -v "not a known color"
