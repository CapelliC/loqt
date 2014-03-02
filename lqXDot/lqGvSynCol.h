/*
    lqXDot        : interfacing Qt and Graphviz library

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

#ifndef GVSYNTAXCOLOR_H
#define GVSYNTAXCOLOR_H

#include "lqXDot_global.h"

#include <QSyntaxHighlighter>
#include <QTextCursor>
#include <QTextEdit>

// very simple - token based - syntax highlighter for DOT format
//
class LQXDOTSHARED_EXPORT lqGvSynCol : public QSyntaxHighlighter
{
    Q_OBJECT
public:

    lqGvSynCol(QObject *parent = 0) : QSyntaxHighlighter(parent) { setup(); }
    lqGvSynCol(QTextDocument *parent) : QSyntaxHighlighter(parent) { setup(); }
    lqGvSynCol(QTextEdit *parent)  : QSyntaxHighlighter(parent) { setup(); }

signals:

    // let caller known we are done
    void completed();

public slots:

protected:

    // handle state tracking using BlockUserData
    virtual void highlightBlock(const QString &text);

    enum token { Comm, Keyw, Attr, Quoted, Number, Edge, Unknown };
    QTextCharFormat fmt[Unknown+1];
    void setup();
};

#endif // GVSYNTAXCOLOR_H
