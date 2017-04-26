#!/bin/bash
echo '----- Install xelatex and required LaTeX libraries. -----'
sudo dnf install -y texlive-collection-fontsrecommended \
                    texlive-xetex \
                    texlive-latex \
                    texlive-babel \
                    texlive-babel-english \
                    texlive-babel-german \
                    texlive-titlesec \
                    texlive-moderncv \
                    texlive-euenc \
                    texlive-relsize \
                    fontawesome-fonts \
                    texlive-fontawesome
echo '----- Install CoffeeScript -----'
sudo dnf install -y coffee-script
echo '----- Install ImageMagick -----'
sudo dnf install -y ImageMagick
echo '----- Update files database -----'
sudo mktexlsr
