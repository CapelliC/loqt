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
#include <QTextBlock>

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

foldedTextAttr::folding foldedTextAttr::fold(QTextCursor c) {
    QTextCharFormat f;
    f.setObjectType(type());
    auto s = c.selection();
    QVariant v; v.setValue(s);
    f.setProperty(prop(), v);
    c.insertText(QString(QChar::ObjectReplacementCharacter), f);
    return folding { s.toPlainText().length(), -1 };
}

QTextDocumentFragment foldedTextAttr::fragment(QTextCharFormat f) {
    Q_ASSERT(f.objectType() == type());
    QVariant v = f.property(prop());
    return v.value<QTextDocumentFragment>();
}

bool foldedTextAttr::unfold(QTextCursor c) {
    if (!c.hasSelection()) {
        QTextCharFormat f = c.charFormat();
        if (f.objectType() == type()) {
            c.movePosition(c.Right);
            if (c.charFormat().objectType() != type())
                c.movePosition(c.Left);
            c.movePosition(c.Left, c.KeepAnchor);
            c.insertFragment(fragment(f));
            return true;
        }
    }
    return false;
}
int foldedTextAttr::unfoldAll() {
    return -1;
}
foldedTextAttr::actualPos foldedTextAttr::cursorPos(QTextCursor c) const {
    return actualPos { 0,0, c.position() };
}
/*
void foldedTextAttr::fragments(QTextCursor c, function<bool()> f) {
    for (QTextBlock i = c.document()->begin(); i != c.document()->end(); i = i.next()) {
        for (QTextBlock::iterator j = i.begin(); j != i.end(); ++j) {
            auto f = j.fragment();
            if (f.charFormat().objectType() == type()) {
                pos += fragment(f.charFormat()).toPlainText().length() - 1;
            }
        }
        if (b == i)
            break;
    }
    return pos;
}
*/
int foldedTextAttr::translatePos(QTextCursor c, int pos) {
    QTextDocument *d = c.document();
    QTextBlock b = d->findBlock(pos);
    for (QTextBlock i = d->begin(), e = d->end(); i != e; i = i.next()) {
        for (QTextBlock::iterator j = i.begin(); j != i.end(); ++j) {
            auto f = j.fragment();
            if (f.charFormat().objectType() == type()) {
                pos += fragment(f.charFormat()).toPlainText().length() - 1;
            }
        }
        if (b == i)
            break;
    }
    return pos;
}
int foldedTextAttr::offset(QTextCursor c) {
    int offset = 0;
    auto d = c.document();
    auto blockPos = d->findBlock(c.position());
    for (QTextBlock i = d->begin(), e = d->end(); i != e; i = i.next()) {
        for (QTextBlock::iterator j = i.begin(); j != i.end(); ++j) {
            auto f = j.fragment();
            if (f.charFormat().objectType() == type())
                offset += fragment(f.charFormat()).toPlainText().length() - 1;
        }
        if (blockPos == i)
            break;
    }
    return offset;
}
