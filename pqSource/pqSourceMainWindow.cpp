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

#include "pqSourceMainWindow.h"
#include "pqSource.h"
#include "MruHelper.h"
#include "Preferences.h"
#include "PREDICATE.h"
#include "FindReplace.h"
#include "do_events.h"
#include "pqGraphviz.h"

#include <QDebug>
#include <QStatusBar>
#include <QFileDialog>
#include <QMdiSubWindow>
#include <QApplication>
#include <QMenuBar>
#include <QTime>
#include <QTimer>
#include <QMessageBox>
#include <QComboBox>
#include <QWidgetAction>
#include <QStringListModel>
#include <QWebView>

structure1(library)
structure1(atom)
structure1(files)

predicate1(use_module)
structure2(doc_for_file)    // not a predicate
predicate2(message_to_string)
predicate2(with_output_to)

pqSourceMainWindow::pqSourceMainWindow(int argc, char **argv, QWidget *parent)
    : MdiHelper(parent), gui_thread_engine(0)
{
    qDebug() << "pqSourceMainWindow" << CT;

    //pqGraphviz::registerMetaTypes();

    setCentralWidget(new QMdiArea(this));
    setupMdi();

    auto e = new ConsoleEdit(argc, argv);
    connect(e, SIGNAL(engine_ready()), this, SLOT(engine_ready()));
    connect(e, SIGNAL(cursorPositionChanged()), this, SLOT(cursorPositionChanged()));
    e->setLineWrapMode(e->NoWrap);

    mdiArea()->addSubWindow(e)->setWindowTitle("Console");

    /* doesn't work - why ?
    QComboBox *comboBox = new QComboBox(menuBar());
    QWidgetAction *checkableAction = new QWidgetAction(menuBar());
    checkableAction->setDefaultWidget(comboBox);
    menuBar()->addAction(checkableAction);
    */

    findReplace = new FindReplace(this);
    connect(findReplace, SIGNAL(outcome(QString)), statusBar(), SLOT(showMessage(QString)));

    QTimer::singleShot(0, this, SLOT(fixGeometry()));
}

void pqSourceMainWindow::engine_ready() {
    qDebug() << "engine_ready" << CT;

    //SwiPrologEngine::in_thread _it;
    gui_thread_engine = new SwiPrologEngine::in_thread;

    //foreach (auto m, QString("syncol,debug_helper").split(',')) {
    foreach (auto m, QString("syncol,trace_interception").split(',')) {
        //bool rc = _it.resource_module(m);
        bool rc = gui_thread_engine->resource_module(m);
        qDebug() << m << rc;
    }

    pqGraphviz::setup();
}

void pqSourceMainWindow::requestHelp(QString cursorWord) {
    if (QWebView *h = helpView())
        h->setUrl(QString("http://localhost:%1/search?for=%2&in=all&match=summary").arg(helpDocPort).arg(cursorWord));
}

void pqSourceMainWindow::fixGeometry() {
    Preferences p;
    loadMru(p, this);

    p.beginGroup("geometry");
    foreach (QString k, p.allKeys())
        if (k == metaObject()->className())
            restoreGeometry(p.value(k).toByteArray());
            //p.loadPosSizeState(k, this);
        else {
            int x = k.indexOf('/');
            if (x != -1) {
                int i = k.mid(x + 1).toInt();
                if (k.left(x) == ConsoleEdit::staticMetaObject.className()) {
                    auto w = mdiArea()->subWindowList().at(0);
                    w->restoreGeometry(p.value(k).toByteArray());
                    int dy = w->y() == 24 ? 0 : w->y();
                    w->move(w->x(), dy);
                } else if (k.left(x) == pqSource::staticMetaObject.className()) {
                    qApp->postEvent(this, new reqEditSource(files[i], p.value(k).toByteArray()));
                }
            }
        }
    p.endGroup();

    restoreState(p.value("windowState").toByteArray());

    QStringList saved_queries = p.value("queries").toStringList();
    if (saved_queries.isEmpty())
        saved_queries.append(emptyQuery());
    queriesBox->addItems(saved_queries);
    queriesBox->setEditable(true);
}

void pqSourceMainWindow::closeEvent(QCloseEvent *e) {
    Q_UNUSED(e)

    qDebug() << "pqSourceMainWindow::closeEvent";
/*
    foreach (auto w, mdiArea()->subWindowList()) {
        if (auto s = qobject_cast<pqSource*>(w->widget()))
            if (!s->canClose()) {
                e->ignore();
                return;
            }
        if (auto c = qobject_cast<ConsoleEdit*>(w->widget())) {
            if (!c->can_close()) {
                e->ignore();
                return;
            }
        }
    }
*/
    Preferences p;
    storeMru(p);

    p.beginGroup("geometry");
    p.remove("");
    p.setValue(metaObject()->className(), saveGeometry());
    //p.savePosSizeState(metaObject()->className(), this);

    foreach (auto w, mdiArea()->subWindowList()) {
        QString key = w->widget()->metaObject()->className();
        if (auto s = qobject_cast<pqSource*>(w->widget())) {
            // bind to MRU file entry
            key = QString("%1/%2").arg(key).arg(files.indexOf(s->file));
        }
        if (auto c = qobject_cast<ConsoleEdit*>(w->widget())) {
            // bind to console Prolog thread id
            key = QString("%1/%2").arg(key).arg(c->thread_id());
        }
        p.setValue(key, w->saveGeometry());
        //p.savePosSizeState(key, w);
    }
    p.endGroup();

    // save for later quit_request(), that issues a PL_halt, then issues a $rl_history
    foreach (auto w, mdiArea()->subWindowList())
        if (auto c = qobject_cast<ConsoleEdit*>(w->widget()))
            pqConsole::last_history_lines = c->history_lines();

    mdiArea()->closeAllSubWindows();
    if (mdiArea()->currentSubWindow()) {
        e->ignore();
        return;
    }

    p.setValue("windowState", saveState());

    QStringList l;
    for (int i = 0; i < queriesBox->count(); ++i)
        l.append(queriesBox->itemText(i));
    p.setValue("queries", l);

    if (!SwiPrologEngine::quit_request())
        e->ignore();
}

void pqSourceMainWindow::customEvent(QEvent *event) {
    Q_ASSERT(event->type() == QEvent::User+1);
    auto res = static_cast<reqEditSource*>(event);
    emit openFile(res->file, res->geometry, res->line, res->linepos);
}

inline QString prologFiles() { return QObject::tr("Prolog files (*.pl *.plt *.pro)"); }
inline QString prologSuffix() { return "pl"; }

void pqSourceMainWindow::openFiles() {
    foreach(QString p,
            QFileDialog::getOpenFileNames(this, tr("Open Files"), QString(),
                prologFiles() + tr(";;All files (*.*)")))
        openFile(p);
}

void pqSourceMainWindow::openFile(QString path, QByteArray geometry, int line, int linepos) {

    foreach (QMdiSubWindow *w, mdiArea()->subWindowList())
        if (auto s = qobject_cast<pqSource*>(w->widget()))
            if (s->file == path) {
                s->placeCursor(line, linepos);
                return;
            }

    if (QFile::exists(path)) {

        auto e = new pqSource(path);
        e->setLineWrapMode(e->NoWrap);

        connect(e, SIGNAL(reportInfo(QString)), statusBar(), SLOT(showMessage(QString)));
        connect(e, SIGNAL(reportInfo(QString)), SLOT(reportInfo(QString)));
        connect(e, SIGNAL(reportError(QString)), SLOT(reportError(QString)));

        connect(e, SIGNAL(cursorPositionChanged()), SLOT(cursorPositionChanged()));
        connect(e, SIGNAL(requestHelp(QString)), SLOT(requestHelp(QString)));

        insertPath(this, path);
        auto w = mdiArea()->addSubWindow(e);
        if (!geometry.isEmpty()) {
            w->restoreGeometry(geometry);
            int dy = w->y() == 24 ? 0 : w->y();
            w->move(w->x(), dy);
        }

        w->show();

        e->loadSource(line, linepos);
        insertPath(this, path);
    }
    else {
        QMessageBox::critical(this, tr("Error"), tr("File %1 not found").arg(path));
        removePath(this, path);
    }
}

void pqSourceMainWindow::newFile() {
    QFileDialog dia(this, tr("Create Prolog Source"), QString(), prologFiles());
    dia.setAcceptMode(dia.AcceptSave);
    dia.setDefaultSuffix(prologSuffix());
    if (dia.exec()) {
        auto path = dia.selectedFiles()[0];
        QFile f(path);
        if (f.open(QFile::WriteOnly|QFile::Text)) {
            {   QTextStream s(&f);
                QString module = QFileInfo(path).baseName();
                if (module[0].isUpper() || module.contains(' '))
                    module = "'" + module + "'";
                QString user = QFileInfo(qApp->applicationFilePath()).owner();
                s << QString(
                     "/** <module> %2\n"
                     " *\n"
                     " *  file %1, created at %3\n"
                     " *  user %4\n"
                     " */\n\n"
                     ":- module(%2, [%2/0]).\n\n"
                     "%2 :- writeln(%2).\n").arg(path, module, QDateTime::currentDateTime().toString(), user);
                                                 //QTime::currentTime().toString(), user);
            }
            openFile(path);
        }
    }
}

void pqSourceMainWindow::openFile() {
    QFileDialog dia(this, tr("Select Prolog Source(s)"), QString(), prologFiles());
    dia.setFileMode(dia.ExistingFiles);
    if (dia.exec())
        foreach (auto f, dia.selectedFiles())
            openFile(f);
}

void pqSourceMainWindow::openFileIndex(int index) {
    openFile(files[index]);
}

void pqSourceMainWindow::make() {
    auto l = matching_sources([](pqSource* s) { return s->is_modified(); });
    if (!l.isEmpty()) {
        QMessageBox mb(this);
        mb.setIcon(mb.Warning);
        mb.setInformativeText(tr("Do you want to save ?"));
        if (l.count() == 1) {
            mb.setText(tr("source '%1' has been modified.").arg(l[0]->file));
            mb.setStandardButtons(mb.Yes | mb.Cancel);
            mb.setDefaultButton(mb.Yes);
        }
        else {
            QStringList lt;
            foreach (auto s, l)
                lt << s->file;
            mb.setText(tr("sources <ul><li>%1</ul> have been modified.").arg(lt.join("<li>")));
            mb.setStandardButtons(mb.YesToAll | mb.Cancel);
            mb.setDefaultButton(mb.YesToAll);
        }
        if (mb.exec() == mb.Cancel)
            return;
        foreach (pqSource* s, l)
            if (!s->saveSource())
                return;
    }

    PlCall("make");
}

void pqSourceMainWindow::saveFile() {
    if (auto s = activeChild<pqSource>())
        s->saveSource();

    if (auto c = activeChild<ConsoleEdit>()) {
        QFileDialog dia(this, tr("Save Console as HTML"), QString(), tr("HTML Files (*.html)"));
        dia.setAcceptMode(dia.AcceptSave);
        dia.setDefaultSuffix("html");
        if (dia.exec()) {
            QFile f(dia.selectedFiles()[0]);
            if (f.open(f.WriteOnly|f.Text))
                f.write(c->toHtml().toUtf8());
        }
    }
}

void pqSourceMainWindow::saveFileAs() {
    qDebug() << "saveFileAs";
    if (auto s = qobject_cast<pqSource*>(activeMdiChild())) {
        QFileDialog dia(this, tr("Save Prolog Source"), QString(), prologFiles());
        dia.setAcceptMode(dia.AcceptSave);
        dia.setDefaultSuffix(prologSuffix());
        if (dia.exec()) {
            if (s->saveSourceAs(dia.selectedFiles()[0]))
                insertPath(this, s->file);
        }
    }
}

void pqSourceMainWindow::helpStart()
{
    QString msg;
    try {
        reportInfo(tr("starting doc_server at port %1").arg(helpDocPort));

        if (PlCall("use_module(library(pldoc))")) {
            if (PlCall(QString("doc_server(%1)").arg(helpDocPort).toUtf8())) {

                //
                if (PlCall("use_module(library(pldoc/doc_library))")) {
                    if (PlCall("doc_load_library"))
                        reportInfo(tr("doc_load_library at port %1 ok").arg(helpDocPort));
                }
                //

                if (QWebView *w = helpView()) {
                    w->setUrl(QString("http://localhost:%1").arg(helpDocPort));
                    reportInfo(tr("doc_server started at port %1").arg(helpDocPort));
                }
                else
                    msg = tr("cannot open helpView");
            }
            else
                msg = tr("cannot start doc_server at port %1").arg(helpDocPort);
        }
        else
            msg = tr("library(pldoc) not available");
    }
    catch(PlException e) {
        msg = t2w(e);
    }

    if (!msg.isEmpty())
        reportError(msg);
}

/*
void pqSourceMainWindow::helpStart()
{
    QString msg;
    try {
        reportInfo(tr("starting doc_server at port %1").arg(helpDocPort));

        if (PlCall("use_module(library(pldoc))")) {
            if (PlCall(QString("doc_server(%1)").arg(helpDocPort).toUtf8())) {

                //
                if (PlCall("use_module(library(pldoc/doc_library))")) {
                    if (PlCall("doc_load_library"))
                        reportInfo(tr("doc_load_library at port %1 ok").arg(helpDocPort));
                }
                //

                if (QWebView *w = helpView()) {
                    w->setUrl(QString("http://localhost:%1").arg(helpDocPort));
                    reportInfo(tr("doc_server started at port %1").arg(helpDocPort));
                }
                else
                    msg = tr("cannot open helpView");
            }
            else
                msg = tr("cannot start doc_server at port %1").arg(helpDocPort);
        }
        else
            msg = tr("library(pldoc) not available");
    }
    catch(PlException e) {
        msg = t2w(e);
    }

    if (!msg.isEmpty())
        reportError(msg);
}
*/

// make resources available
//
struct pldocBrowser : QTextBrowser {
    virtual QVariant loadResource(int type, const QUrl &name) {
        QString respath(":/prolog/pldoc/" + name.toString());
        switch (type) {
        case QTextDocument::HtmlResource:
        case QTextDocument::StyleSheetResource: {
            QFile f(respath);
            if (f.open(f.ReadOnly))
                return f.readAll();
        }   break;
        case QTextDocument::ImageResource:
            QImage i;
            if (i.load(respath))
                return i;
            break;
        }
        qDebug() << "loadResource failed" << type << name;
        return false;
    }
};

void pqSourceMainWindow::helpDoc()
{
    if (auto s = qobject_cast<pqSource*>(activeMdiChild())) {
        //SwiPrologEngine::in_thread t;
        T html, options, anvar;
        L options_(options); options_.append(::files(anvar)); options_.close();

        try {
            if (use_module(library(A("pldoc/doc_html"))) &&
                    with_output_to(atom(html), doc_for_file(A(s->file), options))) {
                QTextBrowser *b = new pldocBrowser;
                b->setHtml(t2w(html));
                mdiArea()->addSubWindow(b);
                b->show();
            }
        }
        catch(PlException e) {
            QMessageBox b(this);
            b.setText(tr("PlException running doc_for_file/2"));
            b.setInformativeText(t2w(e));
            b.setIcon(b.Critical);
            b.exec();
        }
    }
}

struct tag {
    tag(QString t, QTextStream &s) : t(t),s(s)
    {
        s << '<' << t << '>';
    }
    ~tag()
    {
        s << '<' << '/' << t << '>';
    }

    QString t;
    QTextStream &s;
};
struct table : tag { table(QTextStream &s) : tag("table", s) {}};
struct td : tag { td(QTextStream &s) : tag("td", s) {}};
struct tr : tag { tr(QTextStream &s) : tag("tr", s) {}};

void pqSourceMainWindow::about() {
    //SwiPrologEngine::in_thread it;
    QMessageBox info(this);
    PlTerm S;
    if (message_to_string(A("about"), S)) {
        /*
        info.setText(QString("<p><img src=\":/swipl.png\"></img>%1</p><hr>%2").arg(P).arg(tr("<p><ol>"
            "<li>pqConsole: SWI-Prolog interface to Qt</li>"
            "<li>pqSource : SWI-Prolog source goodies</li></ol>"
            "</p> by <i><a href=\"mailto:cc.carlo.cap@gmail.com\">Carlo Capelli</a></i>")));
        */
#define TD  "<td align=center>"
        info.setText(QString(
            "<table>"
            "<tr>"TD"<img src=':/swipl.png'></td>"                                          TD"%1</td></tr>"
                         "<tr></tr>"
            "<tr>"TD"<a href='https://github.com/CapelliC/pqConsole'>pqConsole</a></td>"    TD"SWI-Prolog interface to Qt</td></tr>"
            "<tr>"TD"<a href=''>pqGraphviz</a></td>"                                        TD"SWI-Prolog+Qt interface to Graphviz</td></tr>"
            "<tr>"TD"<a href=''>pqSource</a></td>"                                          TD"SWI-Prolog source goodies</td></tr>"
                         "<tr></tr>"
            "<tr>"TD"by</td>"                                                               TD"<a href=\"mailto:cc.carlo.cap@gmail.com\">ing. Carlo Capelli</td></tr>"
            "</table>").arg(t2w(S)));
    }
    info.exec();
}

QString pqSourceMainWindow::symbol(QWidget *w) {

    if (auto *e = qobject_cast<ConsoleEdit*>(w))
        return QString(tr("Console %1")).arg(e->thread_id());

    if (auto *s = qobject_cast<pqSource*>(w))
        return path2title(s->file);
        //return QString(tr("Source %1")).arg(path2title(s->file));

    return w->windowTitle();
}

// place the hook in required module
#undef PROLOG_MODULE
#define PROLOG_MODULE "prolog_edit"

PREDICATE(edit_source, 1) {
    qDebug() << "edit_source" << t2w(PL_A1);
    bool rc = false;

    QString file;
    int line = 0, linepos = 0;
    PlTail L(PL_A1); PlTerm head;
    if (L.next(head) && !strcmp(head.name(), "file")) {
        file = t2w(head[1]);
        if (L.next(head) && !strcmp(head.name(), "line")) {
            line = head[1];
            if (L.next(head) && !strcmp(head.name(), "linepos"))
                linepos = head[1];
        }
    }

    if (!file.isEmpty())
        if (ConsoleEdit *e = pqConsole::peek_first()) {
            for (auto p = e->parentWidget(); p; p = p->parentWidget())
                if (auto w = qobject_cast<pqSourceMainWindow*>(p)) {
                    pqConsole::gui_run([&]() {
                        auto r = new pqSourceMainWindow::reqEditSource(file, QByteArray(), line, linepos);
                        QApplication::instance()->postEvent(w, r);
                        rc = true;
                    });
                }
        }
    qDebug() << "return" << rc;
    return rc;
}

void pqSourceMainWindow::consult() {
    if (auto e = activeChild<pqSource>())
        e->consult();
}

/** debug commands */
void pqSourceMainWindow::run() {
    if (auto e = activeChild<pqSource>())
        e->run();
}
void pqSourceMainWindow::stepIn() {
    if (auto e = activeChild<pqSource>())
        e->stepIn();
}
void pqSourceMainWindow::stop() {
    if (auto e = activeChild<pqSource>())
        e->stop();
}
void pqSourceMainWindow::stepOut() {
    if (auto e = activeChild<pqSource>())
        e->stepOut();
}
void pqSourceMainWindow::stepOver() {
    if (auto e = activeChild<pqSource>())
        e->stepOver();
}
void pqSourceMainWindow::toggleBP() {
    if (auto e = activeChild<pqSource>())
        e->toggleBP();
}
void pqSourceMainWindow::watchVar() {
    if (auto e = activeChild<pqSource>())
        e->watchVar();
}
QString pqSourceMainWindow::currentQuery() const {
    return queriesBox->currentText();
}

/** find/replace interface */
void pqSourceMainWindow::find()
{
    if (auto e = activeChild<QTextEdit>())
        findReplace->do_search(e);
}
void pqSourceMainWindow::findNext() {}
void pqSourceMainWindow::findPrevious() {}
void pqSourceMainWindow::replace() {}

/** simple logging to text file
 */
void pqSourceMainWindow::reportToFile(QString msg)
{
    if (!reportFile.isOpen()) {
        reportFile.setFileName("reportFile.txt");
        reportFile.open(QFile::WriteOnly);
    }

    if (reportFile.isOpen())
        QTextStream(&reportFile) << QTime::currentTime().toString() << ":" << msg << endl;
}

/** display msg
 */
void pqSourceMainWindow::reportInfo(QString msg)
{
    reportToFile("i:" + msg);
    do_events();
}

/** display error message box and report to logger
 */
void pqSourceMainWindow::reportError(QString msg)
{
    reportToFile("e:" + msg);

    QMessageBox e(this);
    e.setText(tr("Report Error"));
    e.setIcon(e.Critical);
    e.setInformativeText(msg);
    e.exec();
}

/** find or build the help view
 */
QWebView *pqSourceMainWindow::helpView()
{
    QWebView *w;
    foreach(QMdiSubWindow *s, mdiArea()->subWindowList())
        if ((w = qobject_cast<QWebView*>(s->widget())))
            return w;

    QString msg;
    reportInfo(tr("starting doc_server at port %1").arg(helpDocPort));
    mdiArea()->addSubWindow(w = new QWebView(this));
    w->setWindowTitle(tr("Help (courtesy plDoc)"));
    w->show();

    try {
        if (PlCall("use_module(library(pldoc))")) {
            if (PlCall(QString("doc_server(%1)").arg(helpDocPort).toUtf8())) {
                /*if (PlCall("use_module(library(pldoc/doc_library))")) {
                    if (PlCall("doc_load_library"))
                        reportInfo(tr("doc_load_library at port %1 ok").arg(helpDocPort));
                }*/
                w->setUrl(QString("http://localhost:%1").arg(helpDocPort));
                reportInfo(tr("doc_server started at port %1").arg(helpDocPort));
            }
            else
                msg = tr("cannot start doc_server at port %1").arg(helpDocPort);
        }
        else
            msg = tr("library(pldoc) not available");
    }
    catch(PlException e) {
        msg = t2w(e);
    }

    if (!msg.isEmpty())
        reportError(msg);
    return w;
}

/** make a XREF report in graph shape for current source
 */
void pqSourceMainWindow::viewGraph()
{
    qDebug() << "viewGraph";
    /*if (auto e = activeChild<pqSource>()) {
        auto x = new pqXRef();
        x->setWindowTitle(tr("Graph - ") + e->file);
        auto m = mdiArea()->addSubWindow(x);
        m->show();
    }
        */
}

/** get a list of all sources matching bool inspect(pqSource)
 */
QList<pqSource*> pqSourceMainWindow::matching_sources(std::function<bool(pqSource*)> inspect)
{
    QList<pqSource*> l;
    foreach (auto w, mdiArea()->subWindowList())
        if (auto s = qobject_cast<pqSource*>(w->widget()))
            if (inspect(s))
                l << s;
    return l;
}
