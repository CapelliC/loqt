/*
    pqSource     : interfacing SWI-Prolog source files and Qt

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

#ifndef PQHIGHLIGHTER_H
#define PQHIGHLIGHTER_H

#include <QSyntaxHighlighter>
#include <QTextBlock>
#include <QPointer>

#include <QMutex>
#include <QMutexLocker>

#include "pqSyntaxData.h"
#include "pqMiniSyntax.h"

/** highlighter specialized to handle nested categories
 *  categorization performed by SWI-Prolog library(syntax_colour)
 */
class pqHighlighter : public pqMiniSyntax, public pqSyntaxData {
    Q_OBJECT

public:

    pqHighlighter(QTextEdit *host);
    pqHighlighter(QPlainTextEdit *host);

    //! reapply to lines
    void rehighlightLines(ParenMatching::range position);

    //! apply rules to requested text block (say, a line...)
    void highlightBlock(const QTextBlock &b);

    //! if matching, highlight
    void test_highlighting(QTextCursor c);

    //! in case parenthesis have been matched ...
    void clear_highlighting();

    //! clear current variables highlighting
    void clear_hvars();

    void scan_start();
    void scan_done();
    bool sem_info_avail() const { return status == completed; }

    //! editing helper - highlight variables on cursor position
    void cursorPositionChanged(QTextCursor c);

    //! get 'the breadcrumbs' of current under cursor element
    QStringList elementPath(QTextCursor c) const;

    //! get cursor element to edit
    QString elementEdit(QTextCursor c) const;

    //! access toplevel text having position
    QString get_clause_at(int position) const;

    //! from semantic tagging, if available
    QString get_predicate_indicator(QTextCursor c) const;

    //! FindReplace duty
    Q_SLOT void markCursor(QTextCursor c);

    //! get vars of current <p> clause
    QStringList vars(QTextCursor p) const;

    //! get info of predicate head under cursor
    struct predicateHead {
        QString functor;
        int arity;
        QStringList vars;
    };

    //! if succeed, place cursor at position where to insert structured comment
    bool getPredicateHead(predicateHead &ph, QTextCursor &c) const;

protected:

    //! serialize access to UserBlockData formatting
    enum {idle, scanning, completed} status;

    //! apply configuration. Relies on currentBlock() to access actual text position
    virtual void highlightBlock(const QString &text);

    //! change attributes on range based on collected semantic analysis
    void set_sem_attrs(int p, int c, const t_nesting &nest);

    //! change underline of categorized area
    void underline(const cat* c, bool u);

    //! categorized area text cursor
    QTextCursor area(const cat* c) const { return area(*c); }
    QTextCursor area(range r) const;

    QTextCursor onech(int p) const { return area(range(p, p + 1)); }
    QTextCursor onech(QTextCursor p) const { return onech(p.position()); }

    //! fetch categorized area text
    QString text(const cat* c) const;

    void pairchf(range r, QTextCharFormat f) {
        onech(r.beg).setCharFormat(f);
        onech(r.end).setCharFormat(f);
    }

    //! remember highlighted match
    range paren;

    //! FindReplace duty
    QTextCursor marker;

signals:

    void highlightComplete();
};

#endif // PQHIGHLIGHTER_H
