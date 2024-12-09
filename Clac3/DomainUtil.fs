﻿module Clac3.DomainUtil

module ToString =
    let inParans s = "(" + s + ")"
    let inSquareParans s = "[" + s + "]"

module Error = 
    let typeError typeString item = failwithf "Expected %s, got %A" typeString item
