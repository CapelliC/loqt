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

#ifndef PQSYNTAXDATA_H
#define PQSYNTAXDATA_H

#include "pqSource_global.h"
#include "ParenMatching.h"

#include <QSyntaxHighlighter>
#include <QTextEdit>
#include <QProgressBar>
#include <QPointer>
#include <QElapsedTimer>

/** this class collects structured data from SWI-Prolog syntax helper
  * as the library releases info in a non-strict lexical order, by means of a callback,
  * the code recovers the nesting and keep a sorted tree
  */
class PQSOURCESHARED_EXPORT pqSyntaxData : public QObject {

    Q_OBJECT

public:

    pqSyntaxData();
    ~pqSyntaxData();

    //! note: the nesting is recursive, and *can* be compiled just given a predeclaration.
    struct cat;
    typedef QVector<cat> t_nesting;

    //! reuse pair of points from ParenMatching class
    typedef ParenMatching::range range;

    /** this recursive data structure it's the heart of syntax report
      * note: no pointers (except inside QVector...)
      */
    struct cat : range {

        cat(int beg = -1, int end = -1) : range(beg, end) {}
        t_nesting nesting;

        //! summarize info as reported from Prolog
        QString desc;

        //! depending on -part of- description, holds a specified format
        QTextCharFormat fmt;

        //! accelerate lookup - since we have a sorted structure, this should speedup positional access
        bool operator<(const cat &c) const { return beg < c.beg; }

        //! debugging helper - dump indented structure
        QString structure(QString indent) const;

        //! sanity check: nesting should always hold
        int check() const;
    };

    typedef t_nesting::const_iterator itc;
    typedef QList<itc> itcs;

    //! from library callback (so called closure)
    void add_element(const char* functor, int from, int len);

    //! enriched with attributes from library(prolog_colour):syntax_colour/2
    void add_element_attr(QString desc, int from, int len, const QTextCharFormat &fmt);

    //! enriched with attributes from library(prolog_colour):syntax_colour/2
    void add_element_sorted(QString desc, int from, int len, const QTextCharFormat &fmt);

    //! editing helper - highlight variables on cursor position
    void cursorPositionChanged(QTextCursor c);

    //! get 'the breadcrumbs' of current under cursor element
    QStringList elementPath(QTextCursor c) const;

    //! get cursor element to edit
    QString elementEdit(QTextCursor c) const;

    //! debug helper - build nested structure
    QString structure() const;

    //! keep the structure aligned while editing
    void contentsChange(int position, int charsRemoved, int charsAdded);

    //! access toplevel text having position
    QString get_clause_at(int position) const;

    //! incremental updating after editing occurs - place new text at position
    void reconcile(int last_change_position, const pqSyntaxData &updated);

    //! a clause can span more than a category area
    range clause_boundary(int position) const;

    //! find the end point of cursor position
    itcs position_path(int position) const;

    //! apply ordering constraint
    static itc find_position(int p, const t_nesting &n);

    //! if matching, highlight
    void test_highlighting(QTextCursor c);

    //! in case parenthesis have been matched ...
    void clear_highlighting();

    //! clear current variables highlighting
    void clear_hvars();

    //! debugging helper: return true if out-of-order
    bool check() const;

    //! get all applicable attributes from position <p> for length <c>
    itcs matched_attrs(int p, int c) const;

signals:

    //! 4. connection point
    void onProgress(int line);

protected:

    //! actual structured representation
    t_nesting cats;

    //! attempt to adjust structure to recover proper nesting
    void insert_nested(cat &inner, t_nesting& nest);

    //! attempt to adjust structure to recover proper nesting
    void insert_sorted(cat &inner, t_nesting& nest);

    //! navigate area hierarchy for range matching
    //void scan_nested(int p, int c, const t_nesting& nest);

    //! remember previous variable highlighting
    QList<const cat*> hvars;

    //! change underline of categorized area
    void underline(const cat* c, bool u);

    //! categorized area text cursor
    QTextCursor area(const cat* c) const { return area(*c); }
    QTextCursor area(range r) const;

    QTextCursor onech(int p) const { return area(range(p, p + 1)); }
    QTextCursor onech(QTextCursor p) const { return onech(p.position()); }

    void pairchf(range r, QTextCharFormat f) {
        onech(r.beg).setCharFormat(f);
        onech(r.end).setCharFormat(f);
    }

    //! fetch categorized area text
    QString text(const cat* c) const;

    //! remember highlighted match
    range paren;

    //! recursive offseting stored data
    void apply_delta(t_nesting &cats, int position, int delta);
};

#endif // PQSYNTAXDATA_H
