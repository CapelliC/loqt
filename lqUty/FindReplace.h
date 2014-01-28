/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014 Carlo Capelli

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

#ifndef FINDREPLACE_H
#define FINDREPLACE_H

#include "lqUty_global.h"
#include "lqPreferences.h"

#include <QDialog>
#include <QPointer>
#include <QTextEdit>
#include <QComboBox>
#include <QCheckBox>
#include <QPushButton>
#include <QTextDocument>
#include <QPlainTextEdit>

/** make an interface exposing shared functionalities
 *  between QTextEdit and QPlainTextEdit
 */
struct EditInterface {

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

/** find/replace in a QPlainTextEdit/QTextEdit buffer
 *  put Qt text framework and QRegExp to work
 */
class LQUTYSHARED_EXPORT FindReplace : public QDialog
{
    Q_OBJECT

public:

    explicit FindReplace(QWidget *parent = 0);
    ~FindReplace();

    //! apply user selection to <target>
    void do_find(EditInterface i);
    void do_findNext(EditInterface i);
    void do_findPrevious(EditInterface i);
    void do_replace(EditInterface i);

    //! check if replace string has been filled
    bool canReplace() const { return !to_replace.currentText().isEmpty(); }

protected:

    EditInterface ei;

    QComboBox to_search, to_replace;
    QPushButton find, findNext, replace, replaceFind, replaceAll;
    QCheckBox regex, wholeWord, caseSensitive, backward;

    QTextDocument::FindFlags flags() const;
    QTextCursor start();

    void mark(QTextCursor c, bool current, bool repl = false);
    void notfound();

signals:

    void outcome(QString s);
    void markCursor(QTextCursor c);

public slots:
    void onFind();
    void onFindNext();
    void onReplace();
    void onReplaceFind();
    void onReplaceAll();
};

#endif // FINDREPLACE_H
