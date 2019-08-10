#!/bin/bash

POST_NAME="$*"
FILE_NAME=${WEBSITE}/$(date "+%Y-%m-%d")-$(echo "$POST_NAME" | tr -cd "[:alnum:][:blank:]-" | tr ' ' - | tr '[:upper:]' '[:lower:]').markdown

cat >$FILE_NAME <<EOL
---
layout: post
title: "$POST_NAME"
date: $(date "+%Y-%m-%d %H:%M")
image:
tags:
  -
---

EOL

code $FILE_NAME
