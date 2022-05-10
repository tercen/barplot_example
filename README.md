# Template ggplot operator

##### Description

The template ggplot operator demonstrates how to generate a barplot image and markdown components based on the input projection.

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, values to be used in the y axis
`x-axis` (optional) | numeric / factor, values to be used in the x axis
`row` (optional) | factor, factor to be used to separate panels by rows 
`column` (optional) | factor, factor to be used to separate panels by columns 
`colors`        | factor, values to be used to color the bars

Input parameters|.
---|---
`input_var`        | parameter description

Output relations|.
---|---
`Image`        | PNG of the plot in the computed tables.

##### Details

The computation is based on the `ggplot2` R package.
