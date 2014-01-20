/*
    spqr          : SWI-Prolog Qt Rendering

    Author        : Carlo Capelli
    E-mail        : cc.carlo.cap@gmail.com
    Copyright (C) : 2013,2014 Carlo Capelli

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

#include "spqrMainWindow.h"
#include "lqPreferences.h"
#include "blockSig.h"
#include "file2string.h"
#include "pqConsole.h"
#include "PREDICATE.h"

#include <QMenu>
#include <QMenuBar>
#include <QSettings>
#include <QFileDialog>
#include <QDateTime>
#include <QApplication>
#include <QDebug>
#include <QLineEdit>
#include <QStatusBar>

/** GUI setup
 */
spqrMainWindow::spqrMainWindow(int argc, char *argv[], QWidget *parent)
    : QMainWindow(parent), MruHelper("spqr")
{
    statusBar()->showMessage(tr("spqr Setup"));

    lqPreferences p;
    p.loadGeometry(this);

    con = new ConsoleEdit(argc, argv);
    connect(con, SIGNAL(engine_ready()), this, SLOT(engineReady()));

    make_tabs();

    QMenu *m = menuBar()->addMenu(tr("&File"));
    m->setStatusTip(tr("Show File operations"));
    m->addAction(tr("&New..."), this, SLOT(newFile()), QKeySequence::New)->setStatusTip(tr("Create a new file"));
    m->addAction(tr("&Open..."), this, SLOT(openFile()), QKeySequence::Open)->setStatusTip(tr("Open an existing file"));
    m->addMenu(mruMenu = new QMenu(tr("Recent &Files...")))->setStatusTip(tr("List recently opened files"));
    loadMru(p, this);
    m->addSeparator();
    m->addAction(tr("&Save"), this, SLOT(saveFile()), QKeySequence::Save)->setStatusTip(tr("Save the document to disk"));
    m->addAction(tr("Save &As..."), this, SLOT(saveFileAs()), QKeySequence::SaveAs)->setStatusTip(tr("Save the document under a new name"));

    m->addSeparator();
    m->addAction(tr("&Quit"), qApp, SLOT(quit()), QKeySequence::Quit)->setStatusTip(tr("Exit the application"));

    menuBar()->addSeparator();
    menuBar()->addAction("&Exec", this, SLOT(execSource()))->setStatusTip(tr("Run Prolog Source code"));
    menuBar()->addAction("&Source", this, SLOT(viewSource()))->setStatusTip(tr("Show the Prolog source window"));
    menuBar()->addAction("&Console", this, SLOT(viewConsole()))->setStatusTip(tr("Show the SWI-Prolog console"));
    menuBar()->addAction("&Help", this, SLOT(viewHelp()))->setStatusTip(tr("Show SWI-Prolog helpDoc"));

    lastDir = p.value("lastDir").toString();
    QString currFile = p.value("fileSource").toString();
    if (argc >= 2)
        currFile = argv[1];
    if (currFile.length())
        openSourceFile(currFile);
}

/** save settings
 */
spqrMainWindow::~spqrMainWindow() {
    lqPreferences p;
    p.saveGeometry(this);
    p.setValue("fileSource", source()->path());
    p.setValue("lastDir", lastDir);
    storeMru(p);
    p.save();
}

/** handle details on application quit
 */
void spqrMainWindow::closeEvent(QCloseEvent *e) {
    if (!checkSave())
        e->ignore();
}

inline QString Prolog_Exts() { return QObject::tr("Prolog (*.lp *.pl *.pro)"); }

/** run selection dialog and run selected file
 */
void spqrMainWindow::openFile() {
    QFileDialog d(this, tr("Open Prolog file"), lastDir, Prolog_Exts());
    if (d.exec()) {
        lastDir = d.directory().path();
        openSourceFile(d.selectedFiles()[0]);
    }
}

/** query user to save file if modified
 */
bool spqrMainWindow::checkSave() {
    if (isWindowModified()) {
        QMessageBox box(this);
        box.setIcon(box.Warning);
        box.setInformativeText(tr("'file '%1' has been modified!").arg(source()->path()));
        box.setText(tr("Do you want to save your changes?"));
        box.setStandardButtons(box.Save|box.Discard|box.Cancel);
        box.setDefaultButton(box.Save);
        switch (box.exec()) {
        case box.Save:
            saveFile();
            break;
        case box.Cancel:
            return false;
        }
    }
    return true;
}

/** make backup and save script file, report errors
 */
void spqrMainWindow::saveFile() {
    if (source()->saveFile()) {
        setWindowModified(false);
        qDebug() << "saveFile";
    }
}

/** get name and make a new file of it
 */
void spqrMainWindow::saveFileAs() {
    QFileDialog fd(this, tr("Save Prolog file as"), lastDir, Prolog_Exts());
    fd.setAcceptMode(fd.AcceptSave);
    fd.setDefaultSuffix("pl");
    if (fd.exec()) {
        source()->setPath(fd.selectedFiles()[0]);
        initSourceTitle(true);
        saveFile();
    }
}

/** create a new Prolog script
 */
void spqrMainWindow::newFile() {
    if (!checkSave())
        return;

    QFileDialog fd(this, tr("New Prolog File"), lastDir, Prolog_Exts());
    fd.setAcceptMode(fd.AcceptSave);
    fd.setDefaultSuffix("lp");
    if (fd.exec()) {
        QString newPath = fd.selectedFiles()[0];
        QString script = file2string(":/prolog/newFileDefaultScript.pl")
                .arg(newPath, QDateTime::currentDateTime().toString(), QFileInfo(newPath).baseName().replace(' ', '_'));
        source()->setPlainText(script);
        source()->setPath(newPath);
        initSourceTitle(true);
        saveFile();
    }
}

/** this is default as required by MruHelper
 */
void spqrMainWindow::openFileIndex(int i) {
    openSourceFile(files[i]);
}

/** setup principal GUI elements
 */
void spqrMainWindow::make_tabs() {
    delete tabs;

    setCentralWidget(tabs = new QStackedWidget_KeybTabs);
    connect(tabs, SIGNAL(currentChanged(int)), SLOT(currentChanged(int)));

    tabs->addWidget(new CodeMirrorFile);
    tabs->addWidget(con);
    tabs->addWidget(new HelpDocView);

    connect(source(), SIGNAL(helpRequestTopic(QString)), SLOT(helpRequest(QString)));
    connect(source(), SIGNAL(textModified()), SLOT(textModified()));
    connect(source(), SIGNAL(userMessage(CodeMirror::messageKind,QString)), SLOT(userMessage(CodeMirror::messageKind,QString)));

    connect(helpDoc(), SIGNAL(loadFinished(bool)), SLOT(adjustLocation()));
    connect(helpDoc(), SIGNAL(titleChanged(QString)), SLOT(adjustTitle()));
    connect(helpDoc(), SIGNAL(loadProgress(int)), SLOT(setProgress(int)));
    connect(helpDoc(), SIGNAL(loadFinished(bool)), SLOT(finishLoading(bool)));
}

/** setup SWI-Prolog embedding
 */
void spqrMainWindow::engineReady() {
    connect(con->engine(), SIGNAL(query_complete(QString,int)), this, SLOT(queryComplete(QString,int)));
    connect(con->engine(), SIGNAL(query_exception(QString,QString)), this, SLOT(queryException(QString,QString)));

    if (false) {
        SwiPrologEngine::in_thread it;
        foreach (auto m, QString("gv_uty").split(',')) {
            bool rc = it.resource_module(m);
            qDebug() << m << rc;
        }
    }
    else {
        QString spqr_prolog_path;
        QStringList l = QCoreApplication::applicationDirPath().split("/");
        if (l.count() > 2) {
            l.removeLast();
            l.removeLast();
            l << "loqt" << "spqr" << "prolog";
            spqr_prolog_path = l.join("/");
        }
        if (QFile::exists(spqr_prolog_path + "/gv_uty.pl"))
            con->engine()->query_run(QString("asserta(user:file_search_path(spqr, '%1'))").arg(spqr_prolog_path));
        else
            errbox(tr("Configuration Error"), tr("Cannot set file_search_path"));
    }

    con->engine()->query_run("use_module(library(pldoc))");
    con->engine()->query_run(QString("doc_server(%1)").arg(DOC_PORT));
}

void spqrMainWindow::queryComplete(QString query, int tot_occurrences) {
    log(QString("queryComplete %1,%2").arg(query).arg(tot_occurrences));
    if (query.indexOf("doc_server") == 0)
        helpDoc()->setUrl(QString("http://localhost:%1").arg(DOC_PORT));
}

void spqrMainWindow::queryException(QString functor, QString exmsg) {
    err(QString("Query Exception %1 - %2").arg(functor, exmsg));
}

void spqrMainWindow::helpRequest(QString topic) {
    helpDoc()->setUrl(QString("http://localhost:%1/search?for=%2&in=all&match=summary").arg(DOC_PORT).arg(topic));
    viewHelp();
}

/** reinitialize GUI with required script
 */
void spqrMainWindow::openSourceFile(QString file) {
    if (checkSave()) {
        if (!source()->loadFile(file))
            removePath(this, file);
        else {
            initSourceTitle(false);
            viewSource();
            qApp->postEvent(source(), new QKeyEvent(QEvent::KeyPress, Qt::Key_Tab, Qt::NoModifier));
        }
    }
}

void spqrMainWindow::initSourceTitle(bool saved) {
    insertPath(this, source()->path());
    setWindowTitle(QString("%1[*]").arg(QFileInfo(source()->path()).fileName()));
    setWindowModified(saved);
}

/** keep modified status updated
 */
void spqrMainWindow::textModified() {
    setWindowModified(true);
}

/** warn on externally modified script
 */
void spqrMainWindow::scriptChanged(QString path) {
    MB req(MB::Warning, tr("Warning"), tr("Script has been modified externally"), MB::Yes|MB::Ignore, this);
    req.setInformativeText(tr("Do you want to reload '%1'").arg(path));
    if (req.exec() == MB::Yes)
        openSourceFile(path);
}

/** factorize common usage
 */
QMessageBox::StandardButton spqrMainWindow::errbox(QString msg, QString info, MB::StandardButtons buttons, MB::Icon ic, QString title) {
    if (title.isEmpty())
        title = tr("Error");
    MB box(ic, title, msg, buttons, this);
    box.setInformativeText(info);
    return MB::StandardButton(box.exec());
}

void spqrMainWindow::adjustLocation() {
    if (locationEdit)
        locationEdit->setText(helpDoc()->url().toString());
}

void spqrMainWindow::changeLocation() {
    QUrl url = QUrl(locationEdit->text());
    helpDoc()->load(url);
    helpDoc()->setFocus();
}

void spqrMainWindow::adjustTitle() {
    if (progress <= 0 || progress >= 100)
        statusBar()->showMessage(helpDoc()->title());
    else
        statusBar()->showMessage(QString("%1 (%2%)").arg(helpDoc()->title()).arg(progress));
}

void spqrMainWindow::setProgress(int p) {
    progress = p;
    adjustTitle();
}

void spqrMainWindow::finishLoading(bool) {
    progress = 100;
    adjustTitle();
}

/** show feedback on current active panel
 */
void spqrMainWindow::currentChanged(int tabIndex) {
    switch (tabIndex) {
    case t_source:
        statusBar()->showMessage(QString("source: %1").arg(source()->path()));
        break;
    case t_console:
        statusBar()->showMessage("SWI-Prolog console");
        break;
    case t_helpdoc:
        statusBar()->showMessage(helpDoc()->title());
        break;
    }
}

void spqrMainWindow::execSource() {
    auto e = con->engine();
    e->query_run(QString("consult('%1')").arg(source()->path()));
    e->query_run(QFileInfo(source()->path()).baseName());
}

void spqrMainWindow::viewSource() {
    tabs->setCurrentIndex(t_source);
}
void spqrMainWindow::viewConsole() {
    tabs->setCurrentIndex(t_console);
}
void spqrMainWindow::viewHelp() {
    tabs->setCurrentIndex(t_helpdoc);
}

void spqrMainWindow::log(QString msg) {
    qDebug() << msg;
}
void spqrMainWindow::msg(QString msg) {
    log(msg);
    statusBar()->showMessage(msg);
}
void spqrMainWindow::err(QString msg) {
    log(msg);
    errbox(tr("Error"), msg);
}

void spqrMainWindow::userMessage(CodeMirror::messageKind kind, QString s)
{
    switch(kind) {
    case CodeMirror::msg: msg(s); break;
    case CodeMirror::err: err(s); break;
    case CodeMirror::log: log(s); break;
    }
}

/** editing on request from prolog
 *  place the hook in required module
 */
#undef PROLOG_MODULE
#define PROLOG_MODULE "prolog_edit"

PREDICATE(edit_source, 1) {
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
                if (auto w = qobject_cast<spqrMainWindow*>(p)) {
                    pqConsole::gui_run([&]() {
                        QApplication::instance()->postEvent(w, new spqrMainWindow::reqEditSource(file, line, linepos));
                        rc = true;
                    });
                    break;
                }
        }
    return rc;
}

/** consume the edit request event
 */
void spqrMainWindow::customEvent(QEvent *event) {
    Q_ASSERT(event->type() == QEvent::User+1);
    auto res = static_cast<reqEditSource*>(event);
    if (source()->path() != res->file && !checkSave())
        return;
    openSourceFile(res->file);
}
