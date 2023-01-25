/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016,2017,2018,2019

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

#include "lqPreferences.h"

#include <QDialog>
#include <QComboBox>
#include <QCheckBox>
#include <QPushButton>
#include "EditInterface.h"

/** find/replace in a QPlainTextEdit/QTextEdit buffer
 *  put Qt text framework and QRegularExpression to work
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
    void do_findAll(EditInterface i);
    void do_replace(EditInterface i);
    void do_replaceAll(EditInterface i);

    //! check if replace string has been filled
    bool canReplace() const { return !to_replace.currentText().isEmpty(); }

    //! findAll matched
    QList<QTextEdit::ExtraSelection> allMarks;

    //! show matched cursor highlighted
    static void showMatch(QTextCursor c);

protected:

    EditInterface ei;

    QComboBox to_search, to_replace;
    QPushButton find, findNext, findAll, replace, replaceFind, replaceAll;
    QCheckBox regex, wholeWord, caseSensitive, backward;

    QTextDocument::FindFlags flags() const;
    QTextCursor start();

    void mark(QTextCursor c, bool current, bool repl = false);
    void notfound();
    void delayAction(const char *slot);

signals:

    void outcome(QString s);
    void markCursor(QTextCursor c);

public slots:
    void onFind();
    void onFindNext();
    void onFindAll();
    void onReplace();
    void onReplaceFind();
    void onReplaceAll();
};

#endif // FINDREPLACE_H
