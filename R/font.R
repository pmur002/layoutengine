
## Font definitions to use when laying out HTML

## Convert logical italic to character style
fontStyle <- function(italic) {
    ifelse(italic, "italic", "normal")
}

## Convert logical bold to character weight
fontWeight <- function(bold) {
    ifelse(bold, "bold", "normal")
}

cssFontFace <- function(name, style, weight, file) {
     paste("@font-face {",
           paste0('  font-family: "', name, '";'),
           paste0("  font-style: ", style, ";"),
           paste0("  font-weight: ", weight, ";"),
           paste0("  src: url('", basename(file), "');"),
           "}",
           sep="\n")
}

## Turn generic font families into specific fonts
mapGenericFonts <- function(fonts) {
    generic <- grepl("^(sans|serif|mono)$", fonts)
    if (any(generic)) {
        ## R's Cairo devices perform the following subsitutions (see ?X11)
        genericMap <- c(sans="Helvetica", serif="Times", mono="Courier")
        fonts[generic] <- genericMap[fonts[generic]]
        ## Find best match for substituted font
        ## (to emulate what R graphics device will do)
        fonts[generic] <- sapply(fonts[generic], match_family)
    }
    fonts
}

checkMissingCairoFonts <- function(fonts) {
    missing <- !sapply(fonts, font_family_exists)
    if (any(missing))
        stop(paste0("Font(s) not available: ", fonts[missing]))
}

## Look up system fonts and register all variations 
cairoFontCSS <- function(fonts) {
    sf <- get("sysFonts", envir=layoutEngineEnv)
    fonts <- mapGenericFonts(fonts)
    checkMissingCairoFonts(fonts)
    css <- unlist(lapply(fonts,
                         function(font) {
                             which <- sf$family == font
                             cssFontFace(as.character(sf$family[which]),
                                         fontStyle(sf$slant[which] > 0),
                                         sf$weight[which],
                                         as.character(sf$file[which]))
                         }))
    paste(css, collapse="\n")
}

checkMissingType1Fonts <- function(fonts, fontTable) {
    missing <- !(fonts %in% fontTable$FamilyName)
    if (any(missing)) {
        for (i in which(missing)) {
            ## Try font names with white space removed
            trimmedFont <- gsub(" ", "", fonts[i])
            if (trimmedFont %in% fontTable$FamilyName)
                fonts[i] <- trimmedFont
            else
                stop(paste0("Font(s) not available: ", fonts[missing]))
        }
    }
    fonts
}


## Look up font in extrafont::fonttable and
## register each font face
Type1FontCSS <- function(fonts) {
    ft <- get("fontTable", envir=layoutEngineEnv)
    mappedFonts <- mapGenericFonts(fonts)
    checkedFonts <- checkMissingType1Fonts(mappedFonts, ft)
    css <- unlist(lapply(checkedFonts,
                         function(font) {
                             which <- ft$FamilyName == font
                             cssFontFace(as.character(ft$FamilyName[which]),
                                         fontStyle(ft$Italic[which]),
                                         fontWeight(ft$Bold[which]),
                                         as.character(ft$fontfile[which]))
                         }))
    paste(css, collapse="\n")
}

postscriptFontCSS <- function(fonts) {
    Type1FontCSS(fonts)
}

pdfFontCSS <- function(fonts) {
    Type1FontCSS(fonts)
}    

## Generate CSS @font-face rules based on 'fonts'
fontCSS <- function(fonts, device) {
    if (device == "postscript") {
        postscriptFontCSS(fonts)
    } else if (device == "pdf") {
        pdfFontCSS(fonts)
    } else if (cairoDevice(device)) {
        cairoFontCSS(fonts)
    } else {
        stop(paste0("Device ", device, " unsupported"))
    }
}

Type1FirstFont <- function(fonts) {
    ft <- get("fontTable", envir=layoutEngineEnv)
    mappedFonts <- mapGenericFonts(fonts)
    checkedFonts <- checkMissingType1Fonts(mappedFonts, ft)
    checkedFonts[1]
}

cairoFirstFont <- function(fonts) {
    sf <- get("sysFonts", envir=layoutEngineEnv)
    fonts <- mapGenericFonts(fonts)
    checkMissingCairoFonts(fonts[1])
    fonts[1]
}

## Get first font family (may be translated, e.g., if generic font)
firstFont <- function(fonts, device) {
    if (device %in% c("postscript", "pdf")) {
        Type1FirstFont(fonts)
    } else if (cairoDevice(device)) {
        cairoFirstFont(fonts)
    } else {
        stop(paste0("Device ", device, " unsupported"))
    }
}

Type1FontFiles <- function(fonts) {
    ft <- get("fontTable", envir=layoutEngineEnv)
    mappedFonts <- mapGenericFonts(fonts)
    checkedFonts <- checkMissingType1Fonts(mappedFonts, ft)
    unlist(lapply(checkedFonts,
                  function(font) {
                      which <- ft$FamilyName == font
                      as.character(ft$filename[which])
                  }))
}

cairoFontFiles <- function(fonts) {
    sf <- get("sysFonts", envir=layoutEngineEnv)
    fonts <- mapGenericFonts(fonts)
    checkMissingCairoFonts(fonts)
    unlist(lapply(fonts,
                  function(fontname) {
                      which <- sf$family == fontname
                      as.character(sf$file[which])
                  }))
}

## Generate font file paths based on 'fonts'
fontFiles <- function(fonts, device) {
    if (device %in% c("postscript", "pdf")) {
        Type1FontFiles(fonts)
    } else if (cairoDevice(device)) {
        cairoFontFiles(fonts)
    } else {
        stop(paste0("Device ", device, " unsupported"))
    }
}
