/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016

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

#include "foldedTextAttr.h"
#include <QTextDocumentFragment>
#include <QDebug>

foldedTextAttr::foldedTextAttr(QObject *parent) : QObject(parent) {
}

QSizeF foldedTextAttr::intrinsicSize(QTextDocument *doc, int posInDocument, const QTextFormat &format) {
    Q_UNUSED(doc)
    Q_UNUSED(posInDocument)
    Q_ASSERT(format.type() == format.CharFormat);
    const QTextCharFormat &tf = reinterpret_cast<const QTextCharFormat&>(format);

    QFont fn = tf.font();
    QFontMetrics fm(fn);

    QString s("...");
    QSizeF sz = fm.boundingRect(s).size();
    return sz;
}

void foldedTextAttr::drawObject(QPainter *painter, const QRectF &rect, QTextDocument *doc, int posInDocument, const QTextFormat &format) {

    Q_UNUSED(doc)
    Q_UNUSED(posInDocument)
    Q_ASSERT(format.type() == format.CharFormat);
    QString s("...");
    painter->drawText(rect, s);
    painter->drawRect(rect);
}

Q_DECLARE_METATYPE(QTextDocumentFragment)

void foldedTextAttr::fold(QTextCursor c) {
    QTextCharFormat f;
    f.setObjectType(type());
    QVariant v; v.setValue(c.selection());
    f.setProperty(prop(), v);
    c.insertText(QString(QChar::ObjectReplacementCharacter), f);
}
bool foldedTextAttr::unfold(QTextCursor c) {
    if (!c.hasSelection()) {
        QTextCharFormat f = c.charFormat();
qDebug() << "f" << f.objectType() << f.isValid() << c.position();
        if (f.objectType() == type()) {
            c.movePosition(c.Right);
            QTextCharFormat g = c.charFormat();
            if (g.objectType() == type()) {
qDebug() << "g" << g.objectType() << g.isValid() << c.position();
                c.movePosition(c.Left, c.KeepAnchor);
                QVariant v = g.property(prop());
                auto q = v.value<QTextDocumentFragment>();
                c.insertFragment(q);
                return true;
            }
            c.movePosition(c.Left);
            c.movePosition(c.Left, c.KeepAnchor);
            //c.movePosition(c.Right, c.KeepAnchor);
            QVariant v = f.property(prop());
            auto q = v.value<QTextDocumentFragment>();
            c.insertFragment(q);
            return true;
        }
    }
    return false;
}
