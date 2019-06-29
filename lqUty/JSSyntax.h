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

#ifndef JSSYNTAX_H
#define JSSYNTAX_H

#include <QRegExp>
#include <QTextEdit>
#include <QElapsedTimer>
#include <QPlainTextEdit>
#include <QSyntaxHighlighter>

/** a minimal JavaScript syntax highlighter
 */
class JSSyntax : public QSyntaxHighlighter
{
    Q_OBJECT
public:

    JSSyntax(QObject *parent = 0)    : QSyntaxHighlighter(parent) { setup(); }
    JSSyntax(QTextEdit *parent)      : QSyntaxHighlighter(parent) { setup(); }
    JSSyntax(QTextDocument *parent)  : QSyntaxHighlighter(parent) { setup(); }
    JSSyntax(QPlainTextEdit *parent) : QSyntaxHighlighter(parent) { setup(); }

signals:

public slots:

protected:

    // handle state tracking using currentBlockState/previousBlockState
    virtual void highlightBlock(const QString &text);

private:

    enum token_name {
        Comment,
        Atom,
        Atomq,
        Atombackq,
        String,
        Variable,
        Number,
        Operator,
        CharCode,
        Unknown
    };
    QTextCharFormat fmt[Unknown+1];
    QRegExp tokens;

    void setup();
    QElapsedTimer startToEnd;
};

#endif // JSSYNTAX_H
