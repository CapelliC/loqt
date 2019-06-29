/*
    pqSource     : interfacing SWI-Prolog source files and Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
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

#include "pqTextAttributes.h"
#include "PREDICATE.h"
#include <QColor>
#include <QDebug>
#include <QMap>
#include <QTextFormat>

#define unary(X) PlTerm X; PlCompound X ## _t(#X, X);

// keep hashed attributes by list
//
QTextCharFormat pqTextAttributes::operator [](const PlTerm &attr_list)
{
    QString k = t2w(attr_list);
    QTextCharFormat f;

    attrs2format_t::const_iterator p = attrs2format.find(k);
    if (p == attrs2format.end()) {
        // attributes documented here:
        // http://www.swi-prolog.org/pldoc/doc_for?object=prolog_colour:syntax_colour/2

        unary(colour)
        unary(background)
        unary(bold)
        unary(underline)

        // use unification to match list' elements
        PlTail attrs(attr_list);
        PlTerm attr;
        while (attrs.next(attr)) {
            if (attr = colour_t)
                f.setForeground(plColor2Qt(colour));
            else if (attr = background_t)
                f.setBackground(plColor2Qt(background));
            else if (attr = bold_t)
                f.setProperty(f.FontWeight, QFont::Black);
            else if (attr = underline_t)
                f.setProperty(f.FontUnderline, true);
            else
                qDebug() << "unknown class attribute" << t2w(attr);
        }

        attrs2format.insert(k, f);
    }
    else
        f = p.value();

    return f;
}

// map to named colors from http://www.w3.org/TR/SVG/types.html#ColorKeywords
//
QColor pqTextAttributes::plColor2Qt(const PlTerm& tColor)
{
    QColor c;
    QString plColor = t2w(tColor);
    colorname2color_t::const_iterator p = colorname2color.find(plColor);
    if (p == colorname2color.end()) {
        QString color;

        if (plColor == "navy_blue")         color = "navy"; else
        if (plColor == "red4")              color = "brown"; else
        if (plColor == "darkgoldenrod4")    color = "darkgoldenrod"; else
        if (plColor == "dark_slate_blue")   color = "darkslateblue"; else
        if (plColor == "magenta4")          color = "magenta"; else
        if (plColor == "dark_green")        color = "darkgreen"; else
        if (plColor == "grey90")            color = "grey"; else
                                            color = plColor;

        if (QColor::isValidColor(color))
            c = color;
        else
            qDebug() << "invalid" << plColor << color;

        // anyway, avoid repeating the useless test/translation
        colorname2color.insert(color, c);
    }
    else
        c = p.value();

    return c;
}

#undef PROLOG_MODULE
#define PROLOG_MODULE "pqSource"

// debugging: this callback just for test
//
PREDICATE(class_attributes, 2)
{
    static pqTextAttributes ta;
    auto cf = ta[PL_A2];
    qDebug() << t2w(PL_A1) << cf;
    return TRUE;
}
