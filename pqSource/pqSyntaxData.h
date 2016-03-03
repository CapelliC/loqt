/*
    pqSource     : interfacing SWI-Prolog source files and Qt

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

#ifndef PQSYNTAXDATA_H
#define PQSYNTAXDATA_H

#include "pqSource_global.h"
#include "ParenMatching.h"

#include <QSyntaxHighlighter>
#include <QTextEdit>
#include <QProgressBar>
#include <QPointer>
#include <QElapsedTimer>

#include <functional>

/** this class collects structured data from SWI-Prolog syntax helper
  * as the library releases info in a non-strict lexical order, by means of a callback,
  * the code recovers the nesting and keep a sorted tree
  */
class PQSOURCESHARED_EXPORT pqSyntaxData {

public:

    //! note: the nesting is recursive, and *can* be compiled just given a predeclaration.
    struct cat;
    typedef QVector<cat> t_nesting;

    //! reuse pair of points from ParenMatching class
    typedef ParenMatching::range range;

    //! make explicit structure symbols exchanged as atoms
    enum categories {
        structured_comment,
        directive,
        fullstop,
        clause,
        grammar_rule,
        __other__
    };

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

        //! avoid reying on strings
        categories qualification = __other__;

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

    //! debug helper - build nested structure
    QString structure() const;

    //! keep the structure aligned while editing
    void contentsChange(int position, int charsRemoved, int charsAdded);

    //! incremental updating after editing occurs - place new text at position
    void reconcile(int last_change_position, const pqSyntaxData &updated);

    //! a clause can span more than a category area
    range clause_boundary(int position) const;

    //! find the end point of cursor position
    itcs position_path(int position) const;

    //! apply ordering constraint
    static itc find_position(int p, const t_nesting &n);

    //! debugging helper: return true if out-of-order
    bool check() const;

    //! it's a basic visit
    void topdown_preorder(const cat& c, std::function<void(const cat &)>) const;

    //! get text begin/end positions
    range clause_extent(int position) const;

    //! need readonly access to build on structure...
    const t_nesting& nesting() const { return cats; }

    //! folding changes the text buffer... let it know
    void fold(range r);

protected:

    //! actual structured representation
    t_nesting cats;

    //! attempt to adjust structure to recover proper nesting
    void insert_nested(cat &inner, t_nesting& nest);

    //! attempt to adjust structure to recover proper nesting
    void insert_sorted(cat &inner, t_nesting& nest);

    //! remember previous variable highlighting
    QList<const cat*> hvars;

    //! recursive offseting stored data
    void apply_delta(t_nesting &cats, int position, int delta);
};

#endif // PQSYNTAXDATA_H
