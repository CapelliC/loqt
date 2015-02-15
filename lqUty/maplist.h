/*
    lqUty        : loqt utilities

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

#ifndef MAPLIST_H
#define MAPLIST_H

#include <QList>
#include <QString>
#include <QStringList>

#include <functional>

template <class T = QString>
inline QList<T> mapQList(std::function<T(const T&)> pred, const QList<T> &l) {
    QList<T> r;
    foreach(const T& e, l)
        r << pred(e);
    return r;
}

inline QStringList mapQStrings(std::function<QString(const QString&)> pred, const QStringList &l) {
    return mapQList(pred, l);
}

#endif // MAPLIST_H
