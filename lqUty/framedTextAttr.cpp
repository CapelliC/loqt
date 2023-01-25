/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright @ 2023

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

#include "framedTextAttr.h"
#include <QDebug>

framedTextAttr::framedTextAttr(QObject *parent) : QObject(parent) {
}

QSizeF framedTextAttr::intrinsicSize(QTextDocument *doc, int posInDocument, const QTextFormat &format) {
    Q_UNUSED(doc)
    Q_UNUSED(posInDocument)
    Q_ASSERT(format.type() == format.CharFormat);

    return QSizeF(0, 0);
}

void framedTextAttr::drawObject(QPainter *painter, const QRectF &rect, QTextDocument *doc, int posInDocument, const QTextFormat &format) {
    Q_UNUSED(doc)
    Q_UNUSED(posInDocument)
    Q_ASSERT(format.type() == format.CharFormat);
    const QTextCharFormat &tf = reinterpret_cast<const QTextCharFormat&>(format);

    QFont fn = tf.font();
    QFontMetrics fm(fn);

    QString s = format.property(prop()).toString();
    QSizeF sz = fm.boundingRect(s).size();

    QRectF Rect(rect.topLeft(), sz);
    Rect.moveTop(rect.top() - sz.height());
    Rect.adjust(0, 3, 0, 3);

    painter->drawRoundedRect(Rect, 4, 4);
}

void framedTextAttr::frame(QTextCursor c) {
    QTextCharFormat f;
    f.setObjectType(type());

    f.setProperty(prop(), c.selectedText());

    //c.clearSelection();
    if (c.selectionEnd() > c.selectionStart()) {
        c.setPosition(c.selectionStart());
    } else {
        c.setPosition(c.selectionEnd());
    }

    c.insertText(QString(QChar::ObjectReplacementCharacter), f);
}

bool framedTextAttr::unframe(QTextCursor c) {
    auto f = c.charFormat();
    if (f.type() == type()) {
        c.deleteChar();
        return true;
    }
    return false;
}
