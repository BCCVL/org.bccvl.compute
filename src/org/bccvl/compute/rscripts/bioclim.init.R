# This is python string template which is currently tightly tied to
# ../bioclim.py
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

.libPaths("{{ rlibdir }}")
wd = "{{ workdir }}"
species = "{{ species }}"
occur.data = "{{ occurence }}"
bkgd.data = {{ '"%s"' % background if background else "NULL" }}
enviro.data.names = {{ strvector(enviro['names']) }}
enviro.data.current = {{ strvector(enviro['data']) }}
enviro.data.type = {{ strvector(enviro['type']) }}
enviro.data.future = {{ strvector(future['data']) }}

model.bioclim = TRUE
project.bioclim = TRUE
evaluate.bioclim = TRUE #boolean to evaluate BIOCLIM algorithm
model.brt = FALSE #boolean to run Boosted regression tree algorithm
project.brt = FALSE #boolean to project Boosted regression tree algorithm
evaluate.brt = FALSE #boolean to evaluate Boosted regression tree algorithm


opt.tails = c("both")
opt.ext = NULL

# model accuracy statistics
# these are available from dismo::evaluate.R NOT originally implemented in biomod2::Evaluate.models.R
dismo.eval.method = c("ODP", "TNR", "FPR", "FNR", "NPP", "MCR", "OR")
# and vice versa
biomod.models.eval.meth = c("KAPPA", "TSS", "ROC", "FAR", "SR", "ACCURACY", "BIAS", "POD", "CSI", "ETS")
