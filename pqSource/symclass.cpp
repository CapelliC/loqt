/*
    pqSource     : interfacing SWI-Prolog source files and Qt

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

#include "symclass.h"

// a very simple dictionary
// index on first letter, then linear match the array
//
symclass::symclass(const char *csl) : starts(26) {
    int idx = 0;
    words = QString(csl).split(",");
    foreach(QString x, words) {
        x = x.trimmed();
        int f = x[0].unicode();
        Q_ASSERT(f >= 'a' && f <= 'z');
        starts[f - 'a'].append(symidx(x, idx++));
    }
}

// peek first char entry
// scan array
//
int symclass::symindex(const char* sym) const {
    int f = sym[0];
    Q_ASSERT(f >= 'a' && f <= 'z');

    foreach (symidx sx, starts[f - 'a'])
        if (sx.first == sym)
            return sx.second;
    return -1;
}

QString symclass::idxsymbol(int idx) {
    return words[idx];
}
