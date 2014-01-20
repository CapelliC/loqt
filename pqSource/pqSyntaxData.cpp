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

#include "pqSyntaxData.h"
#include "pqHighlighter.h"
#include "PREDICATE.h"

#include <QStack>
#include <QtAlgorithms>

pqSyntaxData::pqSyntaxData() {
    //qDebug() << "pqSyntaxData::pqSyntaxData" << CVP(this);
}

pqSyntaxData::~pqSyntaxData() {
    //qDebug() << "pqSyntaxData::~pqSyntaxData" << CVP(this);
    delete pgb;
}

void pqSyntaxData::add_element(const char* functor, int from, int len)
{
    cat c(from, from + len);
    c.desc = functor;
    insert_nested(c, cats);

    if (cats.count() % 10 == 0)
        reportProgress(from);

    foreach (auto c, cats)
        Q_ASSERT(c.check());
}

void pqSyntaxData::add_element_attr(QString desc, int from, int len, QTextCharFormat fmt)
{
    cat c(from, from + len);
    c.fmt = fmt;
    c.desc = desc;

    insert_nested(c, cats);

    if (cats.count() % 10 == 0)
        reportProgress(from);

    foreach (auto c, cats)
        Q_ASSERT(c.check());
}

// recursive insertion, search from back
// relies on non overlapping fragments - i.e. correctly categorized
//
void pqSyntaxData::insert_nested(cat &c, t_nesting& nest)
{
    int inserted = -1;
    for (int p = nest.size() - 1; p >= 0; --p) {

        // fits inside one already stored
        if (nest[p].contains(c.beg)) {
            Q_ASSERT(nest[p].overlap(c.end));
            Q_ASSERT(inserted == -1);
            // recurse inner nesting
            insert_nested(c, nest[p].nesting);
            return;
        }

        // out of order ? insert at same level
        if (nest[p].end <= c.beg) {
            if (inserted == -1)
                nest.insert(p + 1, 1, c);
            return;
        }

        // out of order ? place c at level and make deeper
        if (c.contains(nest[p].beg)) {
            c.nesting.insert(0, 1, nest[p]);
            if (inserted == -1) {
                inserted = p;
                nest[p] = c;
            }
            else
                nest.remove(p, 1);
        }
    }

    // outmost, eventually will be nested later
    if (inserted == -1)
        nest.insert(0, 1, c);
}

// this move from the very beginning to the end, seeking containing nesting
//
void pqSyntaxData::insert_nested_slow(cat &c, t_nesting& nest)
{
    for (int p = 0; p < nest.size(); ++p) {

        // fits inside one already stored
        if (nest[p].contains(c.beg)) {
            insert_nested_slow(c, nest[p].nesting);
            return;
        }

        // out of order ? insert at same level
        if (nest[p].beg > c.end) {
            nest.insert(p, c);
            return;
        }

        // out of order ? place c at level and make deeper
        if (c.contains(nest[p].beg)) {
            c.nesting.append(nest[p]);
            nest[p] = c;
            return;
        }
    }

    // append as outmost, eventually will be nested later
    nest.append(c);
}

// recursive locating in categorized
//
void pqSyntaxData::scan_nested(int p, int c, const t_nesting& nest)
{
    // should use a binary search instead...
    foreach(const cat& x, nest) {
        // f,q cat coords
        int f = x.beg;
        int q = x.end;
        if (c > f && p < q) {
            // offset in block
            int b = std::max(f, p);
            int e = std::min(q, c);

            if (x.fmt.isValid())
                worker->setFormat(b - p, e - b, x.fmt);

            scan_nested(p, c, x.nesting);
        }
    }
    /* this search doesn't works...
    itc P = find_position(p, nest);
    if (P != nest.end()) {
        // f,q cat coords
        int f = P->beg;
        int q = P->end;
        Q_ASSERT(c > f && p < q);

        // offset in block
        int b = std::max(f, p);
        int e = std::min(q, c);

        if (P->fmt.isValid())
            worker->setFormat(b - p, e - b, P->fmt);

        scan_nested(p, c, P->nesting);
    }
    */
}

// factorize out debugging variables highlighting
//
void pqSyntaxData::clear_hvars()
{
    foreach(const cat* x, hvars)
        underline(x, false);
    hvars.clear();
}

void pqSyntaxData::clear_highlighting() {
    if (paren.size()) {
        pairchf(paren, QTextCharFormat());
        paren = range();
    }
}

void pqSyntaxData::test_highlighting(QTextCursor c) {
    ParenMatching m(c);
    if (m)
        (paren = m.positions).format_both(c, paren.bold());
}

// handle dynamic highlighting
//  same clause variables
//  parenthesis matching (TBD check for quoted strings / comments etc)
//
void pqSyntaxData::cursorPositionChanged(QTextCursor c)
{
    // scan top level
    const t_nesting *scan = &cats;
    const cat *inner = 0;

    QStack<const cat*> stcat;
    l:foreach(const cat& x, *scan) {
        if (x.contains(c.position())) {
            inner = &x;
            scan = &x.nesting;
            if (stcat.isEmpty())
                stcat.push(&x);
            goto l;
        }
    }

    //cannot use: prevents steady document display update
    //blockSig bs(worker->document());

    clear_highlighting();

    if (inner && inner->desc == "var") {
        if (!hvars.contains(inner)) {

            clear_hvars();

            QString sym = text(inner);
            while (!stcat.isEmpty()) {
                const cat* t = stcat.pop();
                if (t->desc == "var" && text(t) == sym)
                    hvars.append(t);
                foreach(const cat& x, t->nesting)
                    stcat.push(&x);
            }

            foreach(const cat* x, hvars)
                underline(x, true);
        }
        return;
    }

    clear_hvars();

    test_highlighting(c);
}

// get categorized area (cursor with selection)
//
QTextCursor pqSyntaxData::area(const cat* c) const
{
    return area(*c);
}

// get range area (cursor with selection)
//
QTextCursor pqSyntaxData::area(range r) const
{
    QTextCursor C(worker->document());
    C.setPosition(r.beg);
    C.movePosition(C.NextCharacter, C.KeepAnchor, r.size());
    return C;
}

// get text of categorized area
//
QString pqSyntaxData::text(const cat* c) const { return area(c).selectedText(); }

bool pqSyntaxData::check() const
{
    //qDebug() << "check" << structure();
    foreach (auto c, cats)
        Q_ASSERT(c.check());
    return true;
}

// change underline of categorized area
// this requires signals NOT disabled in document
//
void pqSyntaxData::underline(const cat* c, bool u)
{
    QTextCharFormat f;
    f.setFontUnderline(u);
    area(c).setCharFormat(f);
}

// top down 'breadcrumbs' location of current cursor element
//
QStringList pqSyntaxData::elementPath(QTextCursor c) const
{
    QStringList l;
    const t_nesting *n = &cats;
    _:foreach(const cat& x, *n)
        if (x.contains(c.position())) {
            if (!x.desc.isEmpty())
                l.append(x.desc);
            n = &x.nesting;
            goto _;
        }
    return l;
}

// peek inner nested element
//
QString pqSyntaxData::elementEdit(QTextCursor c) const
{
    const t_nesting *n = &cats;
    const cat *i = 0;
    _:foreach(const cat& x, *n)
        if (x.contains(c.position())) {
            i = &x;
            n = &x.nesting;
            goto _;
        }
    if (i)
        return text(i);
    return QString();
}

// debugging helper, after changing insertion strategy to more efficient one
//
QString pqSyntaxData::cat::structure(QString indent) const
{
    QString r;
    {   QTextStream s(&r);
        s << indent << '{' << beg << ' ' << end << ' '  << desc;
        if (nesting.isEmpty())
            s << '}' << endl;
        else {
            s << ' ' << nesting.size() << endl;
            foreach(auto n, nesting)
                s << n.structure(indent + " ");
            s << indent << '}' << endl;
        }
    }
    return r;
}

// nesting structure sanity check
//
int pqSyntaxData::cat::check() const
{
    if (beg > end)
        return 0;
    if (nesting.isEmpty())
        return 1;
    for (int p = 1; p < nesting.size(); ++p)
        if (nesting[p - 1].end > nesting[p].beg || !nesting[p - 1].check())
            return 0;
    return nesting.back().check();
}

// debugging helper
// required after changing insertion strategy to more efficient one
//
QString pqSyntaxData::structure() const
{
    QString r;
    QTextStream s(&r);
    foreach (auto c, cats)
        s << c.structure("");
    return r;
}

// recursive offsetting stored data
//
void pqSyntaxData::apply_delta(t_nesting &c, int position, int delta)
{
    t_nesting::iterator p = qLowerBound(c.begin(), c.end(), cat(position, position));
    if (p != c.begin())
        --p;
    t_nesting::iterator q = p;
    while (p != c.end()) {
        if (p->beg >= position)
            p->beg += delta;
        if (p->end > position) {
            p->end += delta;
            if (p->end < p->beg)
                p->end = position;
        }

        if (p > q && p->beg < q->end)
            p = c.erase(p);
        else {
            apply_delta(p->nesting, position, delta);
            q = p;
            ++p;
        }
    }
}

// shift stored positions as required by changes
//
void pqSyntaxData::contentsChange(int position, int charsRemoved, int charsAdded)
{
    apply_delta(cats, position, charsAdded - charsRemoved);
    Q_ASSERT(check());
}

// rebuild plain clause' text of clause surrounding position
//
QString pqSyntaxData::get_clause_at(int position) const
{
    QString x;
    if (worker) {
        range cb = clause_boundary(position);
        if (cb.size() > 0 && cb.end < cats.size()) {
            // get plain text
            int start = cats[cb.beg].beg, stop = cats[cb.end].end;
            x = range(start, stop).plainText(worker->document());
        }
    }
    return x;
}

// put back structure from updated, replacing matched
//
void pqSyntaxData::reconcile(int position, const pqSyntaxData &updated)
{
    Q_ASSERT(check());
    Q_ASSERT(updated.check());

    std::function<void(cat &, int)> add_offset = [&](cat &c, int delta) {
        c.beg += delta;
        c.end += delta;
        for (int i = 0; i < c.nesting.size(); ++i)
            add_offset(c.nesting[i], delta);
    };

    if (updated.cats.size() >= 2 && updated.cats[0].desc == "range") {

        const cat &uc = updated.cats[0];
        int uns = uc.nesting.size();

        range cb = clause_boundary(position);
        if (cb.size() > 0 && cb.beg < cats.size()) {
            int p = cats[cb.beg].beg;
            Q_ASSERT(p <= position);
            Q_ASSERT(cb.size() + 1 == updated.cats.size());

            int delta = cats[cb.beg].beg - uc.beg;

            for (int i = 0; i < uns; ++i) {
                cats.insert(cb.beg + i, uc.nesting[i]);
                add_offset(cats[cb.beg + i], delta);
            }
            cats.insert(cb.beg + uns, updated.cats[1]);
            add_offset(cats[cb.beg + uns], delta);

            cats.remove(cb.beg + uc.nesting.size(), cb.size() + 1);
        }
        else {
            t_nesting::iterator p = qLowerBound(cats.begin(), cats.end(), cat(position, position));
            int q = p - cats.begin();

            for (int i = 0; i < uns; ++i) {
                cats.insert(q + i, uc.nesting[i]);
                add_offset(cats[q + i], position);
            }
            cats.insert(q + uns, updated.cats[1]);
            add_offset(cats[q + uns], position);
        }
    }
    else if (updated.cats[0].desc == "comment") {
    }
    else
        Q_ASSERT(false);

    Q_ASSERT(check());
}

// get offsets into description
//
pqSyntaxData::range pqSyntaxData::clause_boundary(int position) const
{
    t_nesting::const_iterator p = qLowerBound(cats.begin(), cats.end(), cat(position, position));
    if (!p->contains(position) && p != cats.begin())
        --p;
    if (p != cats.end() && p->contains(position)) {
        t_nesting::const_iterator dot = p + 1;
        if (dot != cats.end() && dot->size() == 1 && dot->desc == "fullstop")
            // get offsets
            return range(p - cats.begin(), dot - cats.begin());
    }
    return range();
}

pqSyntaxData::itc pqSyntaxData::find_position(int position, const t_nesting &n)
{
    if (!n.isEmpty()) {
        cat c(position, position);
        t_nesting::const_iterator p = qLowerBound(n.begin(), n.end(), c);
        if (p == n.end() || (p->beg > position && p != n.begin()))
            --p;
        if (p->contains(position))
            return p;
    }
    return n.end();
}

// find the nested path of position
//
pqSyntaxData::itcs pqSyntaxData::position_path(int position) const
{
    itcs l;
    const t_nesting *n = &cats;
    for ( ; ; ) {
        itc p = find_position(position, *n);
        if (p != n->end()) {
            l << p;
            n = &p->nesting;
        }
        else
            break;
    }
    return l;
}

void pqSyntaxData::add_element_sorted(QString desc, int from, int len, QTextCharFormat fmt)
{
    cat c(from, from + len);
    c.fmt = fmt;
    c.desc = desc;

    insert_sorted(c, cats);

    if (cats.count() % 10 == 0)
        reportProgress(from);

    foreach (auto c, cats)
        Q_ASSERT(c.check());
}

// recursive insertion, search from back
// relies on non overlapping fragments - i.e. correctly categorized
//
void pqSyntaxData::insert_sorted(cat &c, t_nesting& nest)
{
    int inserted = -1;
    for (int p = nest.size() - 1; p >= 0; --p) {

        // fits inside one already stored
        if (nest[p].contains(c.beg)) {
            if (nest[p].beg == c.beg && nest[p].end < c.end) {
                c.nesting.append(nest[p]);
                nest[p] = c;
                return;
            }

            Q_ASSERT(nest[p].overlap(c.end));
            Q_ASSERT(inserted == -1);
            // recurse inner nesting
            insert_sorted(c, nest[p].nesting);
            return;
        }

        // out of order ? insert at same level
        if (nest[p].end <= c.beg) {
            if (inserted == -1)
                nest.insert(p + 1, 1, c);
            return;
        }

        // out of order ? place c at level and make deeper
        if (c.contains(nest[p].beg)) {
            c.nesting.insert(0, 1, nest[p]);
            if (inserted == -1) {
                inserted = p;
                nest[p] = c;
            }
            else
                nest.remove(p, 1);
        }
    }

    // outmost, eventually will be nested later
    if (inserted == -1)
        nest.insert(0, 1, c);
}
