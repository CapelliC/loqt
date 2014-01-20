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

#include "Preferences.h"
#include "pqSource.h"
#include "PREDICATE.h"
#include "pqSourceMainWindow.h"
#include "CenterWidgets.h"
#include "pqTrace.h"
#include "pqTextAttributes.h"
#include "do_events.h"

#include <QFile>
#include <QMenu>
#include <QDebug>
#include <QTimer>
#include <QTextStream>
#include <QMessageBox>
#include <QCloseEvent>
#include <QtConcurrentRun>
#include <QFutureWatcher>
#include <QInputDialog>

/** from :/prolog/syncol.pl */
predicate2(syncol)

/** from :/prolog/syncol.pl */
predicate4(recolor)

/** from :/prolog/syntax_colours.pl */
predicate2(syntax_colour)

/** from :/prolog/syncol.pl */
predicate2(syncol_allfile)

/** frag(term,int,int,list) */
structure4(frag)

// assume pqConsole has already initialized PlEngine
//
pqSource::pqSource(QString file) :
    state_curr(idle), state_next(idle),
    file(file),
    debugStatus(no_Debug),
    debugCommand(no_Command),
    skip_changes(),
    last_change_position(-1)
    //syn_server(0)
{
    editContext = new QAction(tr("Edit..."), this);
    connect(editContext, SIGNAL(triggered()), this, SLOT(editInvoke()));

    setContextMenuPolicy(Qt::CustomContextMenu);
    connect(this, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(showContextMenu(const QPoint&)));

    connect(this, SIGNAL(cursorPositionChanged()), this, SLOT(cursorPositionChanged()));
    connect(this, SIGNAL(setCallSig(long,long)), this, SLOT(showCall(long,long)));
}

pqSource::~pqSource()
{
    delete hl;
    delete sd;
}

// setup context menu to invoke Prolog edit facility
//
void pqSource::showContextMenu(const QPoint &pt)
{
    QMenu *menu = createStandardContextMenu();
    QTextCursor c = cursorForPosition(pt);
    if (sd && !(editWhat = sd->elementEdit(c)).isEmpty()) {
        /*
        menu->addSeparator();
        menu->addAction(editContext);
        */
        menu->insertAction(menu->actions().at(0), editContext);
        menu->insertSeparator(menu->actions().at(1));
    }
    menu->exec(mapToGlobal(pt));
    delete menu;
}

// relies on edit/1 to invoke proper file editing
//
void pqSource::editInvoke()
{
    //sendCommand(QString("edit(%1).\n").arg(editWhat));
    //PlCall(QString("edit(%1)").arg(editWhat).toUtf8());
    QString msg;
    try {
        if (PlCall("use_module(library(edit))")) {
            PlTerm Spec, FullSpec, Location;
            PlCall("read_term_from_atom", V(A(editWhat), Spec, empty_list()));
            PlQuery q("prolog_edit", "locate", PlTermv(Spec, FullSpec, Location));
            QStringList FullSpecs, Locations;
            while (q.next_solution()) {
                FullSpecs << t2w(FullSpec);
                Locations << t2w(Location);
qDebug() << t2w(FullSpec) << t2w(Location);
            }
            QString editLoc;
            QStringList &ls = FullSpecs;
            if (ls.count() == 1)
                editLoc = ls[0];
            else if (ls.count() > 1)
                editLoc = QInputDialog::getItem(this, tr("Please choice"), tr("select what item to edit"), ls);

            if (!editLoc.isEmpty())
                PlCall(QString("edit(%1)").arg(editLoc).toUtf8());
            else
                msg = tr("nothing to edit for '%1'").arg(editWhat);
        }
        else
            msg = tr("prolog_edit:locate not available from library(edit)");
    }
    catch(PlException e) {
        msg = t2w(e);
    }

    if (!msg.isEmpty())
        QMessageBox::critical(this, tr("Error"), msg);
}

// callback/4 is an interface worth of note: see callback(C) in prolog_colourise_stream/3
// first argument is plSyntaxData
//
PREDICATE(callback, 4)
{
    T Attributes;
    if (syntax_colour(PL_A2, Attributes)) {
        static pqTextAttributes ta;
        static QMutex mtx;
        QMutexLocker lk(&mtx);

        QString kind;
        switch (PL_A2.arity()) {
        case 0:
        case 1:
            kind = t2w(PL_A2);
            break;
        case 2:
        default:
            kind = QString("%1(%2)").arg(PL_A2.name(), PL_A2[1].name());
        }
        pq_cast<pqSyntaxData>(PL_A1)->add_element_attr(kind, PL_A3, PL_A4, ta[Attributes]);
    }
    else
        pq_cast<pqSyntaxData>(PL_A1)->add_element(PL_A2.name(), PL_A3, PL_A4);
    return TRUE;
}

// setup the proper interface to get SWI-Prolog syntax info
//
void pqSource::loadSource(int line, int linepos)
{
    QFile x(file);
    if (!x.open(QIODevice::ReadOnly | QIODevice::Text)) {
        emit reportInfo("can't open " + file);
        return;
    }

    Preferences pref;
    setFont(pref.console_font);

    setText(QTextStream(&x).readAll());
    connect(document(), SIGNAL(contentsChange(int,int,int)), this, SLOT(contentsChange(int,int,int)));

    QString name = QFileInfo(file).fileName();
    parentWidget()->setWindowTitle(name + "[*]");
    placeCursor(line, linepos);

    QTimer::singleShot(100, this, SLOT(startHighliter()));
}

// positioning in source point and bringing to user attention
//
void pqSource::placeCursor(int line, int linepos)
{
    if (line) {
        QTextCursor c = textCursor();
        c.movePosition(c.Start);
        c.movePosition(c.Down, c.MoveAnchor, line - 1);
        if (linepos)
            c.movePosition(c.Right, c.MoveAnchor, linepos - 1);
        setTextCursor(c);
        ensureCursorVisible();
    }
    parentWidget()->setFocus();
}

// start highlighting, using SWI-Prolog syntax analyzer to collect structured data
//
void pqSource::startHighliter()
{
    //startHighliter_old();
    //return;

    // collect structure asyncronously
    auto f = [](QString file, pqSyntaxData* psd) {
        QElapsedTimer tm;
        tm.start();
        SwiPrologEngine::in_thread _it;
        qDebug() << "engine in " << tm.restart();

        try {
            T results, f;
            int rc = syncol_allfile(A(file), results);
            qDebug() << "syncol_allfile" << rc << "in" << tm.restart();

            pqTextAttributes ta;
for (L scanres(results); scanres.next(f); )
    qDebug() << t2w(f);

            for (L scanres(results); scanres.next(f); ) {
qDebug() << t2w(f[1]) << t2w(f[2]) << t2w(f[3]) << t2w(f[4]);
                psd->add_element_sorted(t2w(f[3]), f[1], f[2], ta[f[4]]);
            }

            qDebug() << "add_element_attr" << "in" << tm.elapsed();
        }
        catch(PlException e) {
            qDebug() << t2w(e);
        }
    };

    auto w = new QFutureWatcher<void>;
    connect(w, SIGNAL(finished()), this, SLOT(runHighliter()));

    delete sd;
    delete hl;
    (sd = new pqSyntaxData)->timing.start();
    hl = new pqHighlighter(this);

    // since could be a slow task, place a visual hint to what's going on...
    CenterWidgets(sd->pgb = new QProgressBar(this));

    QTextCursor c = textCursor();
    c.movePosition(c.End);
    sd->pgb->setMaximum(c.position());
    //connect(sd, SIGNAL(onProgress(int)), sd->pgb, SLOT(setValue(int)));
    connect(sd, SIGNAL(onProgress(int)), this, SLOT(onProgress(int)));
    sd->pgb->show();

    // run the Prolog snippet in background (hl pointer)
    w->setFuture(QtConcurrent::run(f, file, sd));
}

void pqSource::runHighliter()
{
    qDebug() << "pqSyntaxData" << file << "done in" << sd->timing.restart();
    sd->pgb->reset();
    sd->pgb->setMaximum(document()->lineCount());

    Q_ASSERT(sd->check());

    hl = new pqHighlighter(this, sd);
    connect(hl, SIGNAL(highlightComplete()), this, SLOT(highlightComplete()));
}

void pqSource::highlightComplete()
{
    qDebug() << "highlight_complete" << file << "done in" << sd->timing.elapsed();
    set_modified(false);
    delete sd->pgb;
}

// start highlighting, using SWI-Prolog syntax analyzer to collect structured data
//
void pqSource::startHighliter_old()
{
    auto f = [](QString file, pqSyntaxData* psd) {
        SwiPrologEngine::in_thread _it;
        try {
            /*int rc = */syncol(A(file), PlTerm(psd));
            //qDebug() << "syncol" << rc;
        }
        catch(PlException e) {
            qDebug() << t2w(e);
        }
    };

    // collect structure asyncronously
    auto w = new QFutureWatcher<void>;
    connect(w, SIGNAL(finished()), this, SLOT(runHighliter()));

    delete sd;
    delete hl;
    (sd = new pqSyntaxData)->timing.start();
    hl = new pqHighlighter(this);

    // since could be a slow task, place a visual hint to what's going on...
    CenterWidgets(sd->pgb = new QProgressBar(this));

    QTextCursor c = textCursor();
    c.movePosition(c.End);
    sd->pgb->setMaximum(c.position());
    connect(sd, SIGNAL(onProgress(int)), this, SLOT(onProgress(int)));
    sd->pgb->show();

    // run the Prolog snippet in background (hl pointer)
    w->setFuture(QtConcurrent::run(f, file, sd));
}

void pqSource::runHighliter_old()
{
    qDebug() << "pqSyntaxData" << file << "done in" << sd->timing.restart();
    sd->pgb->reset();
    sd->pgb->setMaximum(document()->lineCount());

    hl = new pqHighlighter(this, sd);
    connect(hl, SIGNAL(highlightComplete()), this, SLOT(highlightComplete()));
}

void pqSource::highlightComplete_old()
{
    qDebug() << "highlight_complete" << file << "done in" << sd->timing.elapsed();
    set_modified(false);
    delete sd->pgb;
}

void pqSource::onProgress(int p)
{
    if (sd->pgb) {
        sd->pgb->setValue(p);
        do_events();
    }
}

void pqSource::cursorPositionChanged()
{
    if (hl) { // wait for pqSyntaxData filled
        auto c = textCursor();
        emit reportInfo(sd->elementPath(c).join(" / "));
        toggle t(skip_changes);
        sd->cursorPositionChanged(c);
    }
}

// attempt to incrementally colour modified syntax
//
void pqSource::contentsChange(int position, int charsRemoved, int charsAdded)
{
    if (position == 0 && charsRemoved == charsAdded)
        return;
    if (skip_changes)
        return;
    if (!sd || !hl)
        return;

    // reoffset
    sd->contentsChange(position, charsRemoved, charsAdded);

    ParenMatching::range mr(position, charsRemoved, charsAdded);
    pqSyntaxData pqsd;
    T errorPos;

    skip_changes = true;
    try {
        QString cap = sd->get_clause_at(position);
        if (!cap.isEmpty()) {
            if (recolor(A(cap), A(file), &pqsd, errorPos)) {
                if (errorPos.type() == PL_VARIABLE) {
                    sd->reconcile(position, pqsd);
                    hl->rehighlightBlock(document()->findBlock(position));
                    // inform user about symbol change
                    emit cursorPositionChanged();
                }
                else
                    qDebug() << long(errorPos);
            }
        }
        else if (mr.size() > 0) {
            QString cr = mr.linesText(document());
            if (!cr.trimmed().isEmpty()) {
                if (recolor(A(cr), A(file), &pqsd, errorPos)) {
                    if (errorPos.type() == PL_VARIABLE) {
                        sd->reconcile(document()->findBlock(mr.beg).position(), pqsd);
                        hl->rehighlightLines(mr);
                        // inform user about symbol change
                        emit cursorPositionChanged();
                    }
                    else
                        qDebug() << long(errorPos);
                }
            }
        }
    }
    catch(PlException e) {
        qDebug() << "exception" << t2w(e);
    }

    skip_changes = false;

    set_modified(true);
}

void pqSource::closeEvent(QCloseEvent *e)
{
    if (!canClose())
        e->ignore();
    else {
        delete sd;
        delete hl;
    }
}

void pqSource::keyPressEvent(QKeyEvent *e)
{
    if (e->key() == Qt::Key_Tab || e->key() == Qt::Key_Backtab) {
        QTextCursor c = textCursor();
        if (c.hasSelection()) {
            QTextBlock  x = document()->findBlock(c.selectionStart()),
                        y = document()->findBlock(c.selectionEnd());
            if (x.firstLineNumber() < y.firstLineNumber()) {
                int tab2chars = -1;
                while (x < y) {
                    c.setPosition(x.position());
                    if (e->key() == Qt::Key_Backtab) {
                        if (tab2chars == -1)
                            tab2chars = tabStopWidth() / QFontMetrics(c.charFormat().font()).charWidth(" ", 0);
                        QString l = x.text();
                        int p_del = 0;
                        while (p_del < tab2chars && p_del < l.length())
                            if (l[p_del] == '\t') {
                                ++p_del;
                                break;
                            }
                            else if (l[p_del] == ' ')
                                ++p_del;
                            else
                                break;
                        if (p_del > 0) {
                            c.movePosition(c.Right, c.KeepAnchor, p_del);
                            c.removeSelectedText();
                        }
                    }
                    else
                        c.insertText("\t");
                    x = x.next();
                }
                e->ignore();
                return;
            }
        }
    }
    if (e->key() == Qt::Key_Help || e->key() == Qt::Key_F1) {
        QTextCursor c = textCursor();
        c.select(c.WordUnderCursor);
        QString w = c.selectedText();
        if (!w.isEmpty())
            emit requestHelp(w);
    }
    pqSourceBaseClass::keyPressEvent(e);
}

bool pqSource::is_modified() const
{
    return  parentWidget()->isWindowModified();
}
void pqSource::set_modified(bool yes)
{
    parentWidget()->setWindowModified(yes);
}

bool pqSource::canClose()
{
    if (is_modified()) {
        QMessageBox b(this);
        b.setText(tr("Do you want to save your changes?"));
        b.setInformativeText(tr("'%1' has been modified.").arg(file));
        b.setStandardButtons(b.Save | b.Discard | b.Cancel);
        b.setDefaultButton(b.Save);
        switch (b.exec()) {
        case b.Cancel:
            return false;
        case b.Discard:
            return true;
        default:
            state_curr = closing;
            return saveSource();
        }
    }
    return true;
}

bool pqSource::saveSource()
{
    QFile f(file);
    if (f.open(f.WriteOnly|f.Text)) {
        toggle t(skip_changes);

        QTextStream(&f) << toPlainText();
        set_modified(false);

        {   Preferences p;
            QTextCursor c = textCursor();
            c.select(c.Document);
            QTextCharFormat tcf;
            tcf.setFont(p.console_font);
            c.setCharFormat(tcf);
        }

        if (sd) {
            //toggle t(skip_changes);
            sd->clear_highlighting();
            sd->clear_hvars();
        }

        delete sd;
        delete hl;
        if (state_curr == closing)
            return true;
        state_curr = state_next = idle;
        startHighliter();
        return true;
    }
    return false;
}

bool pqSource::saveSourceAs(QString newFile)
{
    QFile f(newFile);
    if (f.open(f.WriteOnly|f.Text)) {
        QTextStream(&f) << toPlainText();
        set_modified(false);
        file = newFile;
        return true;
    }
    return false;
}

void pqSource::insertFromMimeData(const QMimeData *source)
{
    qDebug() << "insertFromMimeData" << source;
    QTextCharFormat f;
    f.setFont(Preferences().console_font);
    textCursor().insertText(source->text(), f);
}
