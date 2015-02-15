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

#include "Preferences.h"
#include "pqSource.h"
#include "PREDICATE.h"
#include "pqSourceMainWindow.h"
#include "CenterWidgets.h"
#include "pqTrace.h"
#include "pqTextAttributes.h"
#include "do_events.h"
#include "file2string.h"
#include "thousandsDots.h"
#include "blockSig.h"

#include <QFile>
#include <QMenu>
#include <QDebug>
#include <QTimer>
#include <QTextStream>
#include <QMessageBox>
#include <QCloseEvent>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QInputDialog>
#include <QStringListModel>

// from :/prolog/syncol.pl
predicate2(syncol)

// from :/prolog/syncol.pl
predicate4(recolor)

// from :/prolog/syntax_colours.pl
predicate2(syntax_colour)

// from :/prolog/syncol.pl
predicate2(syncol_allfile)

// frag(term,int,int,list)
structure4(frag)

/**
 * @brief pqSource::pqSource
 *  constructor of an empty source document
 *  assumes pqConsole has already initialized PlEngine
 *  initialize a context menu to handle semantic contextual editing
 *  initialize the highlighter
 * @param file
 */
pqSource::pqSource(QString file) :
    state_curr(idle), state_next(idle),
    file(file),
    debugStatus(no_Debug),
    debugCommand(no_Command),
    skip_changes(),
    last_change_position(-1)
{
    qDebug() << "pqSource::pqSource" << file;

    editContext = new QAction(tr("Edit..."), this);
    connect(editContext, SIGNAL(triggered()), SLOT(editInvoke()));

    setContextMenuPolicy(Qt::CustomContextMenu);
    connect(this, SIGNAL(customContextMenuRequested(const QPoint&)), SLOT(showContextMenu(const QPoint&)));

    connect(this, SIGNAL(cursorPositionChanged()), SLOT(cursorPositionChanged()));

    connect(this, &pqSource::setCallSig, &pqSource::showCall);

    hl = new pqHighlighter(this);
}

/**
 * @brief pqSource::~pqSource
 *  destructor: releases the highlighter
 */
pqSource::~pqSource()
{
    qDebug() << "pqSource::~pqSource" << file;
    delete hl;
}

/**
 * @brief pqSource::showContextMenu
 *  this slot setup context menu to invoke Prolog edit facility
 * @param pt
 *  window point where to show the menu
 */
void pqSource::showContextMenu(const QPoint &pt)
{
    QMenu *menu = createStandardContextMenu();
    QTextCursor c = cursorForPosition(pt);
    if (!(editWhat = hl->elementEdit(c)).isEmpty()) {
        menu->insertAction(menu->actions().at(0), editContext);
        menu->insertSeparator(menu->actions().at(1));
    }
    menu->exec(mapToGlobal(pt));
    delete menu;
}

/**
 * @brief pqSource::editInvoke
 *  relies on edit/1 to invoke proper file editing
 */
void pqSource::editInvoke()
{
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

// open file in requested mode, throw exception on fail
//
QFile &open_file(QFile &f, QIODevice::OpenMode mode = QIODevice::ReadOnly) {
    if (!f.open(mode))
        throw std::runtime_error("cannot open " + f.fileName().toStdString());
    return f;
}

// check only *first* EOL character
//
bool file_DOS_eol(QFile& file) {
    bool DOS = false;
    char c;
    while (file.getChar(&c))
        if (c == '\r') {
            if (file.getChar(&c))
                if (c == '\n')
                    return true;
            break;
        }
        else if (c == '\n')
            break;
    file.seek(0);
    return DOS;
}

// check EOL mode of path file
//
bool file_DOS_eol(QString path) {
    QFile f(path);
    return file_DOS_eol(open_file(f));
}

// setup the proper interface to get SWI-Prolog syntax info
//
void pqSource::loadSource(int line, int linepos)
{
    QFile x(file);
    if (!x.open(QIODevice::ReadOnly)) { // | QIODevice::Text)) {
        reportUser("can't open " + file);
        return;
    }

    reportUser(tr("reading %1 file (size %2)").arg(file, thousandsDots(x.size())));

    bool nl_conv = false;
    try {
        if (file_DOS_eol(x)) {
            if (QMessageBox::question(this, tr("Wrong NL"),
                        tr("The file contains \\r\\n, need to be saved to be correctly read"),
                            QMessageBox::Ok | QMessageBox::Cancel) == QMessageBox::Ok)
                nl_conv = true;
            else
                return;
        }

        Preferences pref;
        setFont(pref.console_font);

        setPlainText(file2string(x));
        /* disable because I can't still figure out
         * why it's called with equals charsRemoved,charsAdded
         * at unpredictable times - I guess it's because of formatting,
         * but seems it's impossible to overcome the problem, given that QAbstractTextDocumentLayout
         * it's useless, and QTextDocumentLayout it's not accessible
        */
        //connect(document(), SIGNAL(contentsChange(int,int,int)), /*this,*/ SLOT(contentsChange(int,int,int)));
    }
    catch(std::exception &e) {
        reportUser(tr("exception: %1").arg(e.what()));
        return;
    }
    catch(...) {
        reportUser(tr("exception"));
        return;
    }

    setTitle();
    placeCursor(line, linepos);

    if (!isProlog(file))
        reportUser(tr("not a Prolog file, reading as text"));
    else {
        int lc = document()->lineCount();
        const int MAX_LINES = 5000;
        if (lc < MAX_LINES) {
            reportUser(tr("highlighting %1 lines").arg(lc));
            startHighliter();
        }
        else
            reportUser(tr("file too big to be highlighted (%1 lines, max. %2)").arg(thousandsDots(lc), thousandsDots(MAX_LINES)));
    }

    if (nl_conv)
        set_modified(nl_conv);
}

void pqSource::setTitle()
{
    QString name = QFileInfo(file).fileName();
    parentWidget()->setWindowTitle(name + "[*]");
}

void pqSource::reportUser(QString info)
{
    emit reportInfo(info);
    do_events();
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
    // collect structure asyncronously
    auto f = [](QString file, pqSyntaxData* psd) {
        QElapsedTimer tm;
        tm.start();
        SwiPrologEngine::in_thread _it;
        //qDebug() << file << "engine in " << tm.restart();

        try {
            T results, f;
            int rc = syncol_allfile(A(file), results);
            qDebug() << file << "syncol_allfile" << rc << "in" << tm.restart();

            pqTextAttributes ta;

            for (L scanres(results); scanres.next(f); ) {
                psd->add_element_sorted(t2w(f[3]), f[1], f[2], ta[f[4]]);
            }

            //qDebug() << file << "done add_element_attr" << "in" << tm.elapsed();
        }
        catch(PlException e) {
            //emit reportError(t2w(e));
        }
    };
    hl->scan_start();
//#define ASYNC_SYNCOL_HIGHLIGHT
#ifdef ASYNC_SYNCOL_HIGHLIGHT
    auto w = new QFutureWatcher<void>;
    connect(w, SIGNAL(finished()), this, SLOT(runHighliter()));

    // run the Prolog snippet in background (hl pointer)
    w->setFuture(QtConcurrent::run(f, file, hl));
#else
    f(file, hl);
    runHighliter();
#endif
}

void pqSource::runHighliter()
{
    //qDebug() << hl->structure();
    hl->scan_done();
    hl->rehighlight();
}

void pqSource::highlightComplete()
{
    qDebug() << "highlight_complete" << file;
    //qDebug() << "highlight_complete" << file << "done in" << sd->timing.elapsed();
    //set_modified(false);
    //delete sd->pgb;
}

void pqSource::cursorPositionChanged()
{
    if (hl->sem_info_avail()) {
        auto c = textCursor();
        reportUser(hl->elementPath(c).join(" / "));
        toggle t(skip_changes);
        hl->cursorPositionChanged(c);
    }
}

/**
 * @brief pqSource::contentsChange
 *  this slot attempts to incrementally colour modified syntax
 * @param position
 *  text position index where change occurs
 * @param charsRemoved
 *  number of characters removed
 * @param charsAdded
 *  number of characters added
 */
void pqSource::contentsChange(int position, int charsRemoved, int charsAdded)
{
    //QTextDocumentLayout *pl = document()->documentLayout();
    //qDebug() << pl->metaObject()->className();

    qDebug() << "contentsChange" << position << charsRemoved << charsAdded;
    /*if (position == 0 && charsRemoved == charsAdded)
        return;
        */
    if (skip_changes)
        return;
    if (!hl->sem_info_avail())
        return;

    // reoffset
    hl->contentsChange(position, charsRemoved, charsAdded);

    ParenMatching::range mr(position, charsRemoved, charsAdded);
    pqSyntaxData pqsd;
    T errorPos;

    skip_changes = true;

    try {
        auto colorize = [&](QString frag) {
            if (recolor(A(frag), A(file), &pqsd, errorPos)) {
                if (errorPos.type() == PL_VARIABLE) {
                    hl->reconcile(position, pqsd);
                    hl->rehighlightBlock(document()->findBlock(position));
                    // inform user about symbol change
                    emit cursorPositionChanged();
                }
                else
                    emit reportError(tr("recolor: error at %1").arg(long(errorPos)));
            }
        };

        QString cap = hl->get_clause_at(position);
        if (!cap.isEmpty()) {
            colorize(cap);
        }
        else if (mr.size() > 0) {
            QString cr = mr.linesText(document());
            if (!cr.trimmed().isEmpty()) {
                colorize(cr);
            }
        }
    }
    catch(PlException e) {
        //qDebug() << "exception" << t2w(e);
    }

    skip_changes = false;

    if (charsRemoved != charsAdded) // try to avoid undue signal...

    set_modified(true);
}

/**
 * @brief pqSource::closeEvent
 *  standard Qt interface handler: check status before close
 *  if source modified, inquiry user about change/continue, then act on <e>
 * @param e
 *  if user wawnt to continue, force ignore processing
 */
void pqSource::closeEvent(QCloseEvent *e)
{
    if (debugStatus == Breaked) {
        QMessageBox b(this);
        b.setText(tr("Currently in break"));
        b.setInformativeText(tr("%1: Ok to quit ?").arg(file));
        b.setStandardButtons(b.Ok | b.Cancel);
        b.setDefaultButton(b.Ok);
        switch (b.exec()) {
        case b.Cancel:
            e->ignore();
            return;
        default:
            throw new PlException;  // abort the query
        }
    }

    if (!canClose())
        e->ignore();
}

/**
 * @brief pqSource::keyPressEvent
 *  virtual event handler for keyboard events
 *  if currently displaying a Completion interface, handle specific keys
 *  else if completion requested, display the Completion interface
 *  else handle block indentation/deindentation of tabs at start of each selected line
 * @param e
 *  the keyboard event data
 */
void pqSource::keyPressEvent(QKeyEvent *e)
{
    using namespace Qt;
    QTextCursor c = textCursor();

    bool on_completion = autocomp && autocomp->popup()->isVisible();
    if (on_completion) {
        // following keys are forwarded by the completer to the widget
        switch (e->key()) {
        case Key_Enter:
        case Key_Return:
        case Key_Escape:
        case Key_Tab:
        case Key_Backtab:
            e->ignore();
            return; // let the completer do default behavior
        default:
            break;
        }

        pqSourceBaseClass::keyPressEvent(e);
        c.movePosition(c.StartOfWord, c.KeepAnchor);
        autocomp->setCompletionPrefix(c.selectedText());
        autocomp->popup()->setCurrentIndex(autocomp->currentIndex());
        return;
    }

    if (e->key() == Key_Space && e->modifiers() == ControlModifier) {
        completerInit(c);
        return;
    }

    if (e->key() == Key_Tab || e->key() == Key_Backtab) {
        if (c.hasSelection()) {
            QTextBlock  x = document()->findBlock(c.selectionStart()),
                        y = document()->findBlock(c.selectionEnd());
            if (x.firstLineNumber() < y.firstLineNumber()) {
                blockSig ds(document());
                int tab2chars = -1;
                while (x < y) {
                    c.setPosition(x.position());
                    if (e->key() == Key_Backtab) {
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
                set_modified(true);
                e->ignore();
                return;
            }
        }
    }

    if (e->key() == Key_Help || e->key() == Key_F1) {
        QString topic;
        if (hl->sem_info_avail())
            topic = hl->get_predicate_indicator(c);
        if (topic == "") {
            c.select(c.WordUnderCursor);
            topic = c.selectedText();
        }
        if (!topic.isEmpty())
            emit requestHelp(topic);
    }

    pqSourceBaseClass::keyPressEvent(e);
}

/**
 * @brief pqSource::onCompletion
 *  this slot gets called after a positive selection of a completion
 * @param completion
 *  the selected completion string
 */
void pqSource::onCompletion(QString completion) {
    int slash = completion.indexOf('/');
    int len = completion.length();
    if (slash == -1)
        slash = len;
    QString rep = completion.mid(0, slash);
    auto c = textCursor();
    c.select(c.WordUnderCursor);
    c.insertText(rep);
}

/**
 * @brief pqSource::completerInit
 *  create or initialize the autocompletion interface, based on current cursor context
 * @param c
 *  the cursor to handle
 */
void pqSource::completerInit(QTextCursor c) {

    reportUser(tr("starting completion, please wait..."));

    QSet<QString> syms;
    Completion::initialize(syms);

    if (hl->sem_info_avail())
        // get vars
        foreach (QString s, hl->vars(c))
            syms.insert(s);

    QStringList sorted = syms.toList();
    sorted.sort();

    if (!autocomp) {
        autocomp = new QCompleter(new QStringListModel(sorted));
        autocomp->setWidget(this);
        autocomp->setCompletionMode(QCompleter::UnfilteredPopupCompletion);
        connect(autocomp, SIGNAL(activated(QString)), SLOT(onCompletion(QString)));
    }
    else {
        auto model = qobject_cast<QStringListModel*>(autocomp->model());
        model->setStringList(sorted);
    }

    if (!c.hasSelection())
        c.select(c.WordUnderCursor);
    QString prefix = c.selectedText();
    autocomp->setCompletionPrefix(prefix);

    QRect cr = cursorRect();
    cr.setWidth(300);
    autocomp->complete(cr);
    autocomp->popup()->setCurrentIndex(autocomp->currentIndex());

    reportUser(tr("completion available, %1 items").arg(sorted.size()));
}

void pqSource::commentClause()
{
    if (hl->sem_info_avail()) {
        pqHighlighter::predicateHead ph;
        QTextCursor c = textCursor();
        if (hl->getPredicateHead(ph, c)) {
            QStringList vars;
            foreach(QString v, ph.vars)
                vars << QString("%  @arg %1 describe %1").arg(v);
            QStringList comment;
            comment << QString("%% %1 is det.").arg(QString(ph.arity ? "%1(%2)" : "%1").arg(ph.functor, ph.vars.join(", ")))
                    << "%"
                    << QString("%  describe %1").arg(ph.functor)
                    << "%"
                    << vars
                    << "%\n";
            c.insertText(comment.join("\n"));
        }
    }
}

structure1(file)
predicate2(module_property)

/**
 * @brief pqSource::moduleName
 *
 * My convention states that a module file must host a module named as the file itself.
 * This schema isn't the same used by SWI-Prolog library, that often changes
 * module name to best fit intended logic.
 *
 * @return the module name
 */
QString pqSource::moduleName() const
{
    QString s;
    T Module;
    if (module_property(Module, ::file(A(file)))) {
        s = t2w(Module);
    }
    else
        qDebug() << "moduleName on" << file << "failed";

    return s;
}

/**
 * @brief pqSource::startWebScript
 *
 * The script must be a module exposing 'module_name(start_server)' method.
 * Issue the call required to start the server.
 *
 * @return the list of available HTTP handlers
 */
QStringList pqSource::startWebScript()
{
    QStringList l;
    QString m = moduleName();
    if (!m.isEmpty()) {
        try {
            if (!PlCall(m.toStdString().c_str()))
                qDebug() << "startWebScript failed on " << m;
            else {
                l << "http://localhost:3456";
            }
        }
        catch(PlException e) {
            QMessageBox::critical(this, tr("Exception"), tr("calling '%1' thrown '%2'").arg(m, t2w(e)));
        }
    }
    return l;
}

bool pqSource::is_modified() const
{
    return  parentWidget()->isWindowModified();
}

void pqSource::set_modified(bool yes)
{
    qDebug() << "set_modified" << yes;
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

        if (hl->sem_info_avail()) {
            hl->clear_highlighting();
            hl->clear_hvars();
        }

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
        file = newFile;
        setTitle();
        set_modified(false);
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
