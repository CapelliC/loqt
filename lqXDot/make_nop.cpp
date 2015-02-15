/*
    lqXDot       : interfacing Qt and Graphviz library

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015

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

#include "make_nop.h"

make_nop::make_nop(QObject *parent) :
    QObject(parent)
{
}
QString make_nop::transform(QString src)
{
    QString h = "digraph mlxgraph {\n";
    if (!src.startsWith(h)) return "";

    QString dst;
    dst = "digraph mlxgraph { splines=true;\n";
    int p = h.length(), q;
    while ((q = src.indexOf(" -> ", p + 1)) > 0) {
        dst += src.mid(p, q - p); p = q;
        if ((q = src.indexOf("pos=\"", p)) == -1) return "";
        dst += src.mid(p, q - p); p = q;
        if ((q = src.indexOf("\"];", p)) == -1) return "";
        p = q + 1;
    }
    return dst + src.mid(p);
}
