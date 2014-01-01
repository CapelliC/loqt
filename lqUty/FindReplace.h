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
#include <QTextDocument>
#include <QTextEdit>
#include <QComboBox>
#include <QCheckBox>
#include <QPushButton>
#include <QPointer>

class LQUTYSHARED_EXPORT FindReplace : public QDialog
{
    Q_OBJECT

public:

    explicit FindReplace(QWidget *parent = 0);
    ~FindReplace();

    void do_search(QTextEdit *target);
    bool canReplace() const { return !to_replace.currentText().isEmpty(); }

protected:

    QPointer<QTextEdit> target;

    QComboBox to_search, to_replace;
    QPushButton find, findNext, replace, replaceFind, replaceAll;
    QCheckBox regex, wholeWord, caseSensitive, backward;

    QTextDocument::FindFlags flags() const;
    QTextCursor start();
    void mark(QTextCursor c, bool current, bool repl = false);

signals:

    void outcome(QString s);

public slots:
    void onFind();
    void onFindNext();
    void onReplace();
    void onReplaceFind();
    void onReplaceAll();
};

#endif // FINDREPLACE_H
