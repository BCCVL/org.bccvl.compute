# This is jinja2 which takes a dictionary to generate input parameters
# for bioclim.R
{% macro strvector(value) -%}
  {%- if value -%}
    c(
    {%- for item in value -%}
      {{ '"%s"'|format(item) -}}
      {{ "," if not loop.last }}
    {%- endfor -%}
    )
  {%- else -%}
    NULL
  {%- endif -%}
{%- endmacro %}

#define the working directory
scriptdir = normalizePath("{{ scriptdir }}")
inputdir =  normalizePath("{{ inputdir }}")
outputdir =  normalizePath("{{ outputdir }}")

# define the lon/lat of the observation records -- 2 column matrix of longitude and latitude
occur.data = "{{ occurrence }}"
#define the the lon/lat of the background / psuedo absence points to use -- 2 column matrix of longitude and latitude
bkgd.data = {{ '"%s"' % background if background else "NULL" }}

enviro.data.current = {{ strvector(enviro['data']) }} #define the current enviro data to use
enviro.data.type = {{ strvector(enviro['type']) }} #type in terms of continuous or categorical

#additional parameters for projecting bioclim
opt.tails = c("{{ tails }}") # default "both"; use to ignore the left or right tail of the percentile distribution ("both", "low", "high"
opt.ext = NULL #an optional extent object to limit the prediction to a sub-region of 'x'

# model accuracy statistics
# these are available from dismo::evaluate.R NOT originally implemented in biomod2::Evaluate.models.R
dismo.eval.method = c("ODP", "TNR", "FPR", "FNR", "NPP", "MCR", "OR")
# and vice versa
biomod.models.eval.meth = c("KAPPA", "TSS", "ROC", "FAR", "SR", "ACCURACY", "BIAS", "POD", "CSI", "ETS")


# model accuracy statistics - combine stats from dismo and biomod2 for consistent output
model.accuracy = c(dismo.eval.method, biomod.models.eval.meth)
# TODO: these functions are used to evaluate the model ... configurable?
