/*
    pqSource     : interfacing SWI-Prolog source files and Qt

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

#include "pqSyntaxData.h"
#include "PREDICATE.h"

#include <QtAlgorithms>
#include <QTextStream>

pqSyntaxData::cat &set_desc(pqSyntaxData::cat &c, QString desc) {
    #define Q(q) if ((c.desc = desc) == #q) c.qualification = pqSyntaxData::q;
    Q(structured_comment) else
    Q(directive) else
    Q(fullstop) else
    Q(clause) else
    Q(grammar_rule)
    #undef Q
    return c;
}

/** insert qualified fragment - usually from library callback (so called closure)
 */
void pqSyntaxData::add_element(const char* functor, int from, int len)
{
    cat c(from, from + len);
    insert_nested(set_desc(c, functor), cats);
}

/** enriched with attributes from library(prolog_colour):syntax_colour/2
 */
void pqSyntaxData::add_element_attr(QString desc, int from, int len, const QTextCharFormat &fmt)
{
    cat c(from, from + len);
    c.fmt = fmt;
    insert_nested(set_desc(c, desc), cats);
}

/** enriched with attributes from library(prolog_colour):syntax_colour/2
 */
void pqSyntaxData::add_element_sorted(QString desc, int from, int len, const QTextCharFormat &fmt)
{
    cat c(from, from + len);
    c.fmt = fmt;
    insert_sorted(set_desc(c, desc), cats);
}

/** recursive insertion, search from back
 *  relies on non overlapping fragments - i.e. correctly categorized
 */
void pqSyntaxData::insert_nested(cat &c, t_nesting& nest)
{
    int inserted = -1;
    for (int p = nest.size() - 1; p >= 0; --p) {

        // fits inside one already stored
        if (nest[p].contains(c.beg)) {
            Q_ASSERT(nest[p].overlap(c.end) || nest[p].desc == "comment");
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

bool pqSyntaxData::check() const
{
    foreach (auto c, cats)
        Q_ASSERT(c.check());
    return true;
}

/** very basic visit code
 */
void pqSyntaxData::topdown_preorder(const pqSyntaxData::cat &c, std::function<void (const pqSyntaxData::cat &)> f) const
{
    f(c);
    foreach(const cat &e, c.nesting)
        topdown_preorder(e, f);
}

/** debugging helper, after changing insertion strategy to more efficient one
 */
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
    if (beg > end || beg < 0)
        return 0;
    if (nesting.isEmpty())
        return 1;
    for (int p = 1; p < nesting.size(); ++p)
        if (nesting[p - 1].end > nesting[p].beg || !nesting[p - 1].check())
            return 0;
    return nesting.back().check();
}

/** debug helper - build nested structure
 *  required after changing insertion strategy to more efficient one
 */
QString pqSyntaxData::structure() const
{
    QString r;
    QTextStream s(&r);
    foreach (auto c, cats)
        s << c.structure("");
    return r;
}

/** recursive offseting stored data
 */
void pqSyntaxData::apply_delta(t_nesting &c, int position, int delta)
{
    t_nesting::iterator p = std::lower_bound(c.begin(), c.end(), cat(position, position));
    if (p != c.begin())
        --p;
    t_nesting::iterator q = p;
    while (p != c.end()) {
        if (p->beg >= position) {
            p->beg += delta;
            if (p->beg < 0)
                p->beg = 0;
        }
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

/** shift stored positions as required by changes
 *  keep the structure aligned while editing
 */
void pqSyntaxData::contentsChange(int position, int charsRemoved, int charsAdded)
{
    apply_delta(cats, position, charsAdded - charsRemoved);
    Q_ASSERT(check());
}

/** put back structure from updated, replacing matched
 *  incremental updating after editing occurs - place new text at position
 */
void pqSyntaxData::reconcile(int position, const pqSyntaxData &updated)
{
    Q_ASSERT(check());
    Q_ASSERT(updated.check());

    //! recursive lambda: nice !
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
            Q_UNUSED(p)
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
            t_nesting::iterator p = std::lower_bound(cats.begin(), cats.end(), cat(position, position));
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
    else if (updated.cats[0].desc == "structured_comment") {
    }
    else
        Q_ASSERT(false);

    Q_ASSERT(check());
}

/** get offsets into description
 *  a clause can span more than a category area
 */
pqSyntaxData::range pqSyntaxData::clause_boundary(int position) const
{
    t_nesting::const_iterator p = std::lower_bound(cats.begin(), cats.end(), cat(position, position));
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

/** apply ordering constraint
 */
pqSyntaxData::itc pqSyntaxData::find_position(int position, const t_nesting &n)
{
    if (!n.isEmpty()) {
        cat c(position, position);
        t_nesting::const_iterator p = std::lower_bound(n.begin(), n.end(), c);
        if (p == n.end() || (p->beg > position && p != n.begin()))
            --p;
        if (p->contains(position))
            return p;
    }
    return n.end();
}

/** find the nested path of position
 */
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

/** recursive insertion, search from back
 *  relies on non overlapping fragments - i.e. correctly categorized
 */
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

            // doesn't work on comments nested in a list ??
            // Q_ASSERT(nest[p].overlap(c.end));
            if (nest[p].overlap(c.end)) {
                Q_ASSERT(inserted == -1);
                // recurse inner nesting
                insert_sorted(c, nest[p].nesting);
            }
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

pqSyntaxData::range pqSyntaxData::clause_extent(int position) const {
    range cb = clause_boundary(position);
    if (cb.size() > 0 && cb.end < cats.size()) {
        // get actual positions
        int start = cats[cb.beg].beg, stop = cats[cb.end].end;
        return range(start, stop);
    }
    return range();
}

void pqSyntaxData::fold(range r) {
    Q_UNUSED(r)
}
