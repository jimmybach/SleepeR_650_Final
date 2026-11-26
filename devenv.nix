{inputs, ...}: {
  # R with the system's base packages (tidyverse, devtools, rmarkdown, knitr,
  # languageserver), pandoc and the prompt label.
  imports = [inputs.nord.devenvModules.r];

  rProject = {
    # This project's own packages, on top of the base set
    packages = rp:
      with rp; [
        conflicted
        srvyr
        nhanesA
        naniar
        psych
        car
      ];
    rstudio.enable = true;
  };
}
