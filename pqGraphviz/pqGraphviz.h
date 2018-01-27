/*
    pqGraphviz   : interfacing SWI-Prolog and Graphviz library

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016,2017,2018

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

#ifndef PQGRAPHVIZ_H
#define PQGRAPHVIZ_H

#include "pqGraphviz_global.h"
#include <graphviz/gvc.h>
#include <QMetaType>

class PQGRAPHVIZSHARED_EXPORT pqGraphviz {
public:
    pqGraphviz();
    static void setup();
};

#endif // PQGRAPHVIZ_H
