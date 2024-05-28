#!/bin/bash

grep -r "Final score" log | awk '{print $3}' | sort -nr | st
