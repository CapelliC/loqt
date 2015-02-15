/*
    pqSource     : interfacing SWI-Prolog source files and Qt

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

#ifndef SYMCLASS_H
#define SYMCLASS_H

#include <QPair>
#include <QVector>
#include <QStringList>

// low tech (relatively low cost) *const* symbol table
//
class symclass {
public:

    // accept a Comma Separed List of symbols
    symclass(const char* csl);

    // return 0-based index of sym, if found, or -1
    int symindex(const char* sym) const;

    // return symbol string
    QString idxsymbol(int idx);

private:

    QStringList words;

    typedef QPair<QString, int> symidx;
    QVector< QVector<symidx> > starts;
};

#endif // SYMCLASS_H
