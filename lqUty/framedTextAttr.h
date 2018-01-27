/*
    lqUty        : loqt utilities

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

#ifndef FRAMEDTEXTATTR_H
#define FRAMEDTEXTATTR_H

#include "lqUty_global.h"

#include <QObject>
#include <QPainter>
#include <QTextDocument>
#include <QTextObjectInterface>

/**
 * @brief The framedTextAttr class
 *  example code to complete my answer to my question on StackOverflow: see
 *  http://stackoverflow.com/questions/19232882/nice-text-formatting-in-qtextedit-like-qtcreator-does
 */
class LQUTYSHARED_EXPORT framedTextAttr : public QObject, public QTextObjectInterface
{
    Q_OBJECT
    Q_INTERFACES(QTextObjectInterface)

public:
    explicit framedTextAttr(QObject *parent = 0);

    static int type() { return QTextFormat::UserObject+1; }
    static int prop() { return 1; }

    QSizeF intrinsicSize(QTextDocument *doc, int posInDocument, const QTextFormat &format);
    void drawObject(QPainter *painter, const QRectF &rect, QTextDocument *doc, int posInDocument, const QTextFormat &format);

    void frame(QTextCursor c);
    bool unframe(QTextCursor c);
};

#endif // FRAMEDTEXTATTR_H
