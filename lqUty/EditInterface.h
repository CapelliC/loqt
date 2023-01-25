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

#ifndef EDITINTERFACE_H
#define EDITINTERFACE_H

#include "lqUty_global.h"

#include <QPointer>
#include <QTextEdit>
#include <QTextDocument>
#include <QPlainTextEdit>

/** make an interface exposing shared functionalities
 *  between QTextEdit and QPlainTextEdit
 */
struct LQUTYSHARED_EXPORT EditInterface {

    EditInterface() {}
    EditInterface(QTextEdit *edit) : edit(edit) {}
    EditInterface(QPlainTextEdit *plain) : plain(plain) {}

    QTextCursor textCursor() const {
        if (edit) return edit->textCursor();
        if (plain) return plain->textCursor();
        return QTextCursor();
    }
    QTextDocument* document() const {
        if (edit) return edit->document();
        if (plain) return plain->document();
        return 0;
    }
    QWidget* widget() const {
        if (edit) return edit;
        if (plain) return plain;
        return 0;
    }

    void setTextCursor(QTextCursor c) {
        if (edit) edit->setTextCursor(c); else
        if (plain) plain->setTextCursor(c);
    }
    void ensureCursorVisible() {
        if (edit) edit->ensureCursorVisible(); else
        if (plain) plain->ensureCursorVisible();
    }

    QPointer<QTextEdit> edit;
    QPointer<QPlainTextEdit> plain;
};

#endif // EDITINTERFACE_H
