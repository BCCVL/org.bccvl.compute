

get.australian.state.shape <- function(file="SLA11aAust.shp", state.code="1")
{
    library(maptools)
    library(mapdata) # this one makes plot(get.nsw.shape()) work
    shape=readShapePoly("SLA11aAust.shp");
    return (shape[shape$STATE_CODE==state.code,]);
}


get.nsw.shape <- function(file="SLA11aAust.shp")
{
    return (get.australian.state.shape(file=file, state.code="1"))
}

get.vic.shape <- function(file="SLA11aAust.shp")
{
    return (get.australian.state.shape(file=file, state.code="2"))
}

get.qld.shape <- function(file="SLA11aAust.shp")
{
    return (get.australian.state.shape(file=file, state.code="3"))
}

get.sa.shape <- function(file="SLA11aAust.shp")
{
    return (get.australian.state.shape(file=file, state.code="4"))
}

get.wa.shape <- function(file="SLA11aAust.shp")
{
    return (get.australian.state.shape(file=file, state.code="5"))
}

get.tas.shape <- function(file="SLA11aAust.shp")
{
    return (get.australian.state.shape(file=file, state.code="6"))
}

get.nt.shape <- function(file="SLA11aAust.shp")
{
    return (get.australian.state.shape(file=file, state.code="7"))
}

get.act.shape <- function(file="SLA11aAust.shp")
{
    return (get.australian.state.shape(file=file, state.code="8"))
}
# other territories
get.ot.shape <- function(file="SLA11aAust.shp")
{
    return (get.australian.state.shape(file=file, state.code="9"))
}
