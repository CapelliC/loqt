/*
    lqUty         : loqt utilities

    Author        : Carlo Capelli
    E-mail        : cc.carlo.cap@gmail.com
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

#ifndef THOUSANDSDOTS_H
#define THOUSANDSDOTS_H

#include <QString>

//! insert dots to group thousands
inline QString thousandsDots(QString s) {
    int l = s.length();
    if (l > 3)
        return thousandsDots(s.left(l - 3)) + '.' + s.right(3);
    return s;
}

//! insert dots to group thousands
inline QString thousandsDots(ulong n) {
    return thousandsDots(QString::number(n));
}

#endif // THOUSANDSDOTS_H
