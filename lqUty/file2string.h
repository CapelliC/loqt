/*
    lqUty         : loqt utilities

    Author        : Carlo Capelli
    E-mail        : cc.carlo.cap@gmail.com
    Copyright (C) : 2013,2014 Carlo Capelli

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

#ifndef FILE2STRING_H
#define FILE2STRING_H

#include <QFile>
#include <QTextStream>
#include <stdexcept>

//! boilerplate to get a file' contents
inline QString file2string(QFile &f, const char *codec = "UTF-8") {
    QTextStream ts(&f);
    ts.setCodec(codec);
    return ts.readAll();
}

//! boilerplate to get a pathname contents
inline QString file2string(QString path, const char *codec = "UTF-8") {
    QFile f(path);
    if (!f.open(f.ReadOnly))
        throw std::runtime_error("cannot open " + path.toStdString());
    return file2string(f, codec);
}

#endif // FILE2STRING_H
