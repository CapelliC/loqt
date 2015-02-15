/*
    pqSource     : interfacing SWI-Prolog source files and Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015

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

#include "pqHighlighter.h"
#include "PREDICATE.h"
#include "SwiPrologEngine.h"

#include <QDebug>
#include <QStack>

pqHighlighter::pqHighlighter(QTextEdit *host) :
    pqMiniSyntax(host),
    status(idle)
{
}
pqHighlighter::pqHighlighter(QPlainTextEdit *host) :
    pqMiniSyntax(host),
    status(idle)
{
}

// coords are offsets in file
//
void pqHighlighter::highlightBlock(const QString &text)
{
    if (status == idle)
        return;

    if (!marker.isNull() && currentBlock() == marker.block()) {
        QTextCharFormat f;
        f.setBackground(Qt::yellow);
        int s = marker.selectionStart(), e = marker.selectionEnd(), b = marker.block().position();
        setFormat(s - b, e - s, f);
        marker = QTextCursor();
        return;
    }

    if (status == completed) {
        QTextBlock b = currentBlock();
        set_sem_attrs(b.position(), b.position() + b.length(), cats);
    }
    else
        pqMiniSyntax::highlightBlock(text);
}

void pqHighlighter::set_sem_attrs(int p, int c, const t_nesting &nest)
{
    foreach(const cat& x, nest) {
        // f,q cat coords
        int f = x.beg;
        int q = x.end;
        if (c > f && p < q) {
            // offset in block
            int b = std::max(f, p);
            int e = std::min(q, c);

            if (x.fmt.isValid())
                setFormat(b - p, e - b, x.fmt);

            set_sem_attrs(p, c, x.nesting);
        }
    }
}

void pqHighlighter::highlightBlock(const QTextBlock &b)
{
    set_sem_attrs(b.position(), b.position() + b.length(), cats);
}

void pqHighlighter::rehighlightLines(ParenMatching::range mr)
{
    mr.normalize();
    QTextBlock
        b = document()->findBlock(mr.beg),
        e = document()->findBlock(mr.end);
    for ( ; b != e; b = b.next())
        highlightBlock(b);
    if (e != document()->end())
        highlightBlock(b);
}

/** handle dynamic highlighting
 *  same clause variables
 *  parenthesis matching (TBD check for quoted strings / comments etc)
 */
void pqHighlighter::cursorPositionChanged(QTextCursor c)
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

/** get range area (cursor with selection)
 */
QTextCursor pqHighlighter::area(range r) const
{
    QTextCursor C(document());
    C.setPosition(r.beg);
    C.movePosition(C.NextCharacter, C.KeepAnchor, r.size());
    return C;
}

/** get text of categorized area
 */
QString pqHighlighter::text(const cat* c) const { return area(c).selectedText(); }

/** change underline of categorized area
 *  this requires signals NOT disabled in document
 */
void pqHighlighter::underline(const cat* c, bool u)
{
    QTextCharFormat f;
    f.setFontUnderline(u);
    area(c).setCharFormat(f);
}

/** factorize out debugging variables highlighting
 */
void pqHighlighter::clear_hvars()
{
    foreach(const cat* x, hvars)
        underline(x, false);
    hvars.clear();
}

void pqHighlighter::clear_highlighting()
{
    if (paren.size()) {
        pairchf(paren, QTextCharFormat());
        paren = range();
    }
}

void pqHighlighter::test_highlighting(QTextCursor c)
{
    ParenMatching m(c);
    if (m)
        (paren = m.positions).format_both(c, paren.bold());
}

void pqHighlighter::scan_start()
{
    cats.clear();
    status = scanning;
}

void pqHighlighter::scan_done()
{
    status = completed;
}

/** top down 'breadcrumbs' location of current cursor element
 */
QStringList pqHighlighter::elementPath(QTextCursor c) const
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

/** peek inner nested element
 */
QString pqHighlighter::elementEdit(QTextCursor c) const
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

/** rebuild plain clause' text of clause surrounding position
 *  access toplevel text having position
 */
QString pqHighlighter::get_clause_at(int position) const
{
    QString x;
    range cb = clause_boundary(position);
    if (cb.size() > 0 && cb.end < cats.size()) {
        // get plain text
        int start = cats[cb.beg].beg, stop = cats[cb.end].end;
        x = range(start, stop).plainText(document());
    }
    return x;
}

predicate3(read_term_from_atom)

/** return functor/arity if available
 *  arity is simply # children of shared parent
 */
QString pqHighlighter::get_predicate_indicator(QTextCursor c) const
{
    itcs pp = position_path(c.position());
    if (pp.size() == 3 && pp[0]->desc == "clause" && pp[1]->desc == "body") {
        SwiPrologEngine::in_thread e;
        try {
            T V, E; L l(E); l.close();
            if (read_term_from_atom(A(pp[2]->desc), V, E))
                if (V.arity() == 2)
                    return QString("%1/%2").arg(text(pp[2])).arg(V[2].arity());
        }
        catch(PlException ex) {
            qDebug() << t2w(ex);
        }
    }
    return "";
}

/** required by FindReplace to change attributes without undue document modification
 */
void pqHighlighter::markCursor(QTextCursor c)
{
    marker = c;
    rehighlightBlock(c.block());
}

/** access clause structure, if available, and scan for all vars
 */
QStringList pqHighlighter::vars(QTextCursor p) const
{
    QSet<QString> vs;
    itcs pp = position_path(p.position());
    if (!pp.isEmpty())
        topdown_preorder(*pp[0], [&](const cat &c) { if (c.desc == "var")  vs << text(&c); });
    return vs.toList();
}

/** get info of predicate head under cursor
 *  if succeed, place cursor at position where to insert structured comment
 */
bool pqHighlighter::getPredicateHead(pqHighlighter::predicateHead &ph, QTextCursor &p) const
{
    itcs pp = position_path(p.position());
    if (pp.size() == 2 && pp[0]->desc == "clause" && pp[1]->desc.left(4) == "head") {

        SwiPrologEngine::in_thread e;
        try {
            T V, E; L l(E); l.close();
            if (read_term_from_atom(A(pp[1]->desc), V, E))
                if (V.arity() == 2) {
                    T F = V[2];
                    ph.functor = F.name();
                    ph.arity = F.arity();
                    for (int c = 1; c < ph.arity; ++c)
                        ph.vars << t2w(F[c]);
                }
        }
        catch(PlException ex) {
            qDebug() << t2w(ex);
        }

        for (int c = 1; c < pp[0]->nesting.size(); ++c) {
            const cat &C = pp[0]->nesting[c];
            if (C.desc == "neck")
                break;
            if (C.desc == "var") {
                if (c - 1 < ph.vars.size())
                    ph.vars[c - 1] = text(&C);
                else
                    ph.vars << text(&C);
            }
        }

        p.setPosition(pp[0]->beg);
        return true;
    }
    return false;
}
