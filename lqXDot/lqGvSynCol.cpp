/*
    lqXDot        : interfacing Qt and Graphviz library

    Author        : Carlo Capelli
    E-mail        : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016,2017,2018,2019

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "lqGvSynCol.h"
#include <QTextDocument>
#include <QRegularExpression>
#include <QSet>
#include <QDebug>

static struct GvKeywords : QSet<QString> {
    GvKeywords() {
        insert("digraph");
        insert("graph");
        insert("subgraph");
        insert("node");
        insert("edge");
    }
}
keyws;

static struct GvAttrs : QSet<QString> {
    GvAttrs() {
        foreach(QString s, QString(
            "Damping,"
            "K,"
            "URL,"
            "area,"
            "arrowhead,"
            "arrowsize,"
            "arrowtail,"
            "aspect,"
            "bb,"
            "bgcolor,"
            "center,"
            "charset,"
            "clusterrank,"
            "color,"
            "colorscheme,"
            "comment,"
            "compound,"
            "concentrate,"
            "constraint,"
            "decorate,"
            "defaultdist,"
            "dim,"
            "dimen,"
            "dir,"
            "diredgeconstraints,"
            "distortion,"
            "dpi,"
            "edgeU"
            "edgehref,"
            "edgetarget,"
            "edgetooltip,"
            "epsilon,"
            "esep,"
            "fillcolor,"
            "fixedsize,"
            "fontcolor,"
            "fontname,"
            "fontnames,"
            "fontpath,"
            "fontsize,"
            "forcelabels,"
            "gradientangle,"
            "group,"
            "headURL,"
            "head_lp,"
            "headclip,"
            "headhref,"
            "headlabel,"
            "headport,"
            "headtarget,"
            "headtooltip,"
            "height,"
            "href,"
            "id,"
            "image,"
            "imagepath,"
            "imagescale,"
            "label,"
            "labelURL,"
            "label_scheme,"
            "labelangle,"
            "labeldistance,"
            "labelfloat,"
            "labelfontcolor,"
            "labelfontname,"
            "labelfontsize,"
            "labelhref,"
            "labeljust,"
            "labelloc,"
            "labeltarget,"
            "labeltooltip,"
            "landscape,"
            "layer,"
            "layerlistsep,"
            "layers,"
            "layerselect,"
            "layersep,"
            "layout,"
            "len,"
            "levels,"
            "levelsgap,"
            "lhead,"
            "lheight,"
            "lp,"
            "ltail,"
            "lwidth,"
            "margin,"
            "maxiter,"
            "mclimit,"
            "mindist,"
            "minlen,"
            "mode,"
            "model,"
            "mosek,"
            "nodesep,"
            "nojustify,"
            "normalize,"
            "nslimit,"
            "nslimit1,"
            "ordering,"
            "orientation,"
            "orientation,"
            "outputorder,"
            "overlap,"
            "overlap_scaling,"
            "pack,"
            "packmode,"
            "pad,"
            "page,"
            "pagedir,"
            "pencolor,"
            "penwidth,"
            "peripheries,"
            "pin,"
            "pos,"
            "quadtree,"
            "quantum,"
            "rank,"
            "rankdir,"
            "ranksep,"
            "ratio,"
            "rects,"
            "regular,"
            "remincross,"
            "repulsiveforce,"
            "resolution,"
            "root,"
            "rotate,"
            "rotation,"
            "samehead,"
            "sametail,"
            "samplepoints,"
            "scale,"
            "searchsize,"
            "sep,"
            "shape,"
            "shapefile,"
            "showboxes,"
            "sides,"
            "size,"
            "skew,"
            "smoothing,"
            "sortv,"
            "splines,"
            "start,"
            "style,"
            "stylesheet,"
            "tailURL,"
            "tail_lp,"
            "tailclip,"
            "tailhref,"
            "taillabel,"
            "tailport,"
            "tailtarget,"
            "tailtooltip,"
            "target,"
            "tooltip,"
            "truecolor,"
            "vertices,"
            "viewport,"
            "voro_margin,"
            "weight,"
            "width,"
            "xlabel,"
            "xlp,"
            "z").split(','))
        insert(s);
    }
}
attrs;

void lqGvSynCol::setup()
{
    fmt[Comm].setForeground(Qt::darkGreen);
    fmt[Number].setForeground(QColor("blueviolet"));
    fmt[Keyw].setForeground(Qt::darkMagenta);
    fmt[Keyw].setFontWeight(QFont::Bold);
    fmt[Attr].setForeground(Qt::blue);
    fmt[Quoted].setForeground(Qt::magenta);
    fmt[Edge].setFontWeight(QFont::Bold);
    fmt[Unknown].setForeground(Qt::darkRed);
}

/** handle nested comments and simple minded attributes
  */
void lqGvSynCol::highlightBlock(const QString &text)
{
    QString begComm("/\\*");
    QString number("\\d+(?:\\.\\d+)?");
    QString symbol("\\w+");
    QString quoted("\"[^\"]*\"");
    QString edges("--|->");

    QRegularExpression
            tokens(QString("(%1)|(%2)|(%3)|(%4)|(%5)|#").arg(begComm, number, symbol, quoted, edges)),
            endComm("\\*/");
    QRegularExpressionMatch
            matchToken,
            matchComm;

    // simple state machine
    if (currentBlock().blockNumber() > 0)
        setCurrentBlockState(previousBlockState());
    else
        setCurrentBlockState(0);

    for (int i = 0, j, l; ; i = j + l)
        if (currentBlockState()) {              // in multiline comment
            if ((j = text.indexOf(endComm, i, &matchComm)) == -1) {
                setFormat(i, text.length() - i, fmt[Comm]);
                break;
            }
            setFormat(i, j - i + (l = 2), fmt[Comm]);
            setCurrentBlockState(0);
        } else {
            if ((j = text.indexOf(tokens, i, &matchToken)) == -1)
                break;
            QStringList ml = matchToken.capturedTexts();
            Q_ASSERT(ml.length() == 5+1);
            if ((l = ml[1].length())) {         // begin multiline comment
                setFormat(j, l, fmt[Comm]);
                setCurrentBlockState(1);
            } else if ((l = ml[2].length())) {  // number
                setFormat(j, l, fmt[Number]);
            } else if ((l = ml[3].length())) {  // symbol
                if (keyws.contains(ml[3]))
                    setFormat(j, l, fmt[Keyw]);
                else if (attrs.contains(ml[3]))
                    setFormat(j, l, fmt[Attr]);
                else
                    setFormat(j, l, fmt[Unknown]);
            } else if ((l = ml[4].length())) {  // quoted
                setFormat(j, l, fmt[Quoted]);
            } else if ((l = ml[5].length())) {  // edge
                setFormat(j, l, fmt[Edge]);
            } else {                            // single line comment
                setFormat(j, text.length() - i, fmt[Comm]);
                break;
            }
        }

    // rather useless - anyway textChanged will fire after completion
    if (currentBlock().blockNumber() == document()->blockCount() - 1)
        emit completed();
}
