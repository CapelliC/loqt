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

#include "MdiHelper.h"
#include "KeyboardMacros.h"

#include <QApplication>
#include <QMenuBar>
#include <QToolBar>
#include <QStatusBar>
#include <QTextEdit>
#include <QPlainTextEdit>
#include <QTextBlock>
#include <QDebug>

void MdiHelper::setupMdi() {

    mdiArea()->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    mdiArea()->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);

    // ?? useless  mdiArea()->setActivationOrder(QMdiArea::ActivationHistoryOrder);

    connect(mdiArea(), SIGNAL(subWindowActivated(QMdiSubWindow*)), this, SLOT(updateMenus()));
    connect(mdiArea(), SIGNAL(subWindowActivated(QMdiSubWindow*)), this, SLOT(subWindowActivated(QMdiSubWindow*)));

    connect(cmdMapper, SIGNAL(mapped(QWidget*)), this, SLOT(setActiveSubWindow(QWidget*)));

    createActions();
    createMenus();
    createToolBars();
    createStatusBar();
    updateMenus();

    setUnifiedTitleAndToolBarOnMac(true);
}

void MdiHelper::updateMenus() {
    QMdiSubWindow *msw = mdiArea()->activeSubWindow();

    bool hasMdiChild = (msw != 0);

    saveAct->setEnabled(hasMdiChild);
    saveAsAct->setEnabled(hasMdiChild);
    pasteAct->setEnabled(hasMdiChild);
    closeAct->setEnabled(hasMdiChild);
    //closeAllAct->setEnabled(hasMdiChild);
    tileAct->setEnabled(hasMdiChild);
    cascadeAct->setEnabled(hasMdiChild);
    nextAct->setEnabled(hasMdiChild);
    previousAct->setEnabled(hasMdiChild);
    separatorAct->setVisible(hasMdiChild);

    bool hasSelection = false;

    if (msw) {
        auto te = qobject_cast<QTextEdit*>(msw->widget());
        auto pte = qobject_cast<QPlainTextEdit*>(msw->widget());
        hasSelection = (te && te->textCursor().hasSelection()) || (pte && pte->textCursor().hasSelection());
    }

    cutAct->setEnabled(hasSelection);
    copyAct->setEnabled(hasSelection);
}

void MdiHelper::updateWindowMenu() {
    windowMenu->clear();
    windowMenu->addAction(closeAct);
    //windowMenu->addAction(closeAllAct);
    //windowMenu->addSeparator();
    windowMenu->addAction(tileAct);
    windowMenu->addAction(cascadeAct);
    windowMenu->addSeparator();
    windowMenu->addAction(nextAct);
    windowMenu->addAction(previousAct);
    windowMenu->addAction(separatorAct);

    QList<QMdiSubWindow *> windows = mdiArea()->subWindowList();
    separatorAct->setVisible(!windows.isEmpty());
    for (int i = 0; i < windows.size(); ++i) {
        QWidget *child = windows.at(i)->widget();

        QString text= tr(i < 9 ? "&%1 %2" : "%1 %2").arg(i + 1).arg(symbol(child));
        QAction *action  = windowMenu->addAction(text);
        action->setCheckable(true);
        action->setChecked(child == activeMdiChild());
        connect(action, SIGNAL(triggered()), cmdMapper, SLOT(map()));
        cmdMapper->setMapping(action, windows.at(i));
    }
}

void MdiHelper::createActions() {
    auto png = [](const char* i) { return QIcon(QString(":/images/%1.png").arg(i)); };

    auto cmd = [png](QObject *target, QPointer<QAction> &a, QString name, const char *slot, QKeySequence ks = QKeySequence(), const char* ico = 0, QString tip = "") {
        if (ico) {
            a = new QAction(png(ico), name, target);
            a->setIconVisibleInMenu(true);
        }
        else
            a = new QAction(name, target);
        a->setShortcut(ks);
        if (!tip.isEmpty())
            a->setStatusTip(tip);

        connect(a, SIGNAL(triggered()), target, slot);
    };
    typedef QKeySequence __;

    cmd(this,   newAct,     tr("&New"),        SLOT(newFile()),         __::New,    "new",  tr("Create a new file"));
    cmd(this,   openAct,    tr("&Open..."),    SLOT(openFile()),        __::Open,   "open", tr("Open an existing file"));
    cmd(this,   saveAct,    tr("&Save"),       SLOT(saveFile()),        __::Save,   "save", tr("Save the document file to disk"));
    cmd(this,   saveAsAct,  tr("Save &As..."), SLOT(saveFileAs()),      __::SaveAs, 0,      tr("Save the document file under a new name"));
    cmd(qApp,   exitAct,    tr("E&xit"),       SLOT(closeAllWindows()), __::Quit,   0,      tr("Exit the application"));

    cmd(this,   cutAct,     tr("Cu&t"),     SLOT(cut()),    __::Cut,    "cut",      tr("Cut the current selection's contents to the clipboard"));
    cmd(this,   copyAct,    tr("&Copy"),    SLOT(copy()),   __::Copy,   "copy",     tr("Copy the current selection's contents to the clipboard"));
    cmd(this,   pasteAct,   tr("&Paste"),   SLOT(paste()),  __::Paste,  "paste",    tr("Paste the clipboard's contents into the current selection"));

    cmd(this,   findAct,        tr("&Find..."),         SLOT(find()),           __::Find,           "edit-find-3",          tr("Select text and search in current document"));
    cmd(this,   findNextAct,    tr("Find &Next"),       SLOT(findNext()),       __::FindNext,       0,                      tr("Search the next occurrence of text"));
    cmd(this,   findPreviousAct,tr("Find &Previous"),   SLOT(findPrevious()),   __::FindPrevious,   0,                      tr("Search the previous occurrence of text"));
    cmd(this,   replaceAct,     tr("&Replace..."),      SLOT(replace()),        __::Replace,        "edit-find-and-replace",tr("Select text to search and replace"));

    cmd(this,   viewSWIPrologPrefAct,   tr("SWI-Prolog &Preferences"),  SLOT(viewSWIPrologPref()),  __(), 0, tr("Edit SWI-Prolog global preferences (restart this application to see changes)"));
    cmd(this,   selectColorsAct,        tr("Select &Colors..."),        SLOT(selectColors()),       __(), 0, tr("Choose the color palette used for text rendering"));
    cmd(this,   selectFontAct,          tr("Select &Font..."),          SLOT(selectFont()),         __(), 0, tr("Choose the font used to display text"));
    cmd(this,   incFontAct,             tr("&Increase Font Size"),      SLOT(incFont()),    __("Ctrl++"), 0, tr("Make characters bigger (Increase Font size)"));
    cmd(this,   decFontAct,             tr("&Decrease Font Size"),      SLOT(decFont()),    __("Ctrl+-"), 0, tr("Make characters smaller (Decrease Font Size)"));

    QMdiArea *mdiArea = this->mdiArea();
    cmd(mdiArea, closeAct,                  tr("Cl&ose"),           SLOT(closeActiveSubWindow()),       __(),               0, tr("Close the active window"));
    cmd(mdiArea, tileAct,                   tr("&Tile"),            SLOT(tileSubWindows()),             __("Ctrl+T"),       0, tr("Tile the windows"));
    cmd(mdiArea, cascadeAct,                tr("&Cascade"),         SLOT(cascadeSubWindows()),          __(),               0, tr("Cascade the windows"));
    cmd(this,    switchLayoutDirectionAct,  tr("Layout &Direction"),SLOT(switchLayoutDirection()),      __(),               0, tr("Switch layout direction"));
    cmd(this,    switchViewModeAct,         tr("&View Mode"),       SLOT(switchViewMode()),             __(),               0, tr("Switch SubWindow/Tabbed view mode"));
    cmd(mdiArea, nextAct,                   tr("Ne&xt"),            SLOT(activateNextSubWindow()),      __::NextChild,      0, tr("Move the focus to the next window"));
    cmd(mdiArea, previousAct,               tr("Pre&vious"),        SLOT(activatePreviousSubWindow()),  __::PreviousChild,  0, tr("Move the focus to the previous window"));

    separatorAct = new QAction(this);
    separatorAct->setSeparator(true);

    // help
    helpStartAct = new QAction(tr("Help..."), this);
    helpStartAct->setStatusTip(tr("Start PlDoc server and display documentation"));
    connect(helpStartAct, SIGNAL(triggered()), this, SLOT(helpStart()));

    helpDocAct = new QAction(tr("Preview &Documentation"), this);
    helpDocAct->setStatusTip(tr("Show the PlDoc HTML for the script"));
    connect(helpDocAct, SIGNAL(triggered()), this, SLOT(helpDoc()));

    viewGraphAct = new QAction(tr("View G&raph"), this);
    viewGraphAct->setShortcut(QKeySequence("Ctrl+R"));
    viewGraphAct->setStatusTip(tr("Display the XREF graph of current source"));
    connect(viewGraphAct, SIGNAL(triggered()), this, SLOT(viewGraph()));

    viewGraphIncl = new QAction(tr("View &Inclusions"), this);
    viewGraphIncl->setShortcut(QKeySequence("Ctrl+I"));
    viewGraphIncl->setStatusTip(tr("Display the XREF inclusions graph of current source"));
    connect(viewGraphIncl, SIGNAL(triggered()), this, SLOT(viewInclusions()));

    commentClauseAct = new QAction(tr("Comment &Predicate"), this);
    commentClauseAct->setShortcut(QKeySequence("Ctrl+P"));
    commentClauseAct->setStatusTip(tr("Write a structured plDoc comment for current predicate head"));
    connect(commentClauseAct, SIGNAL(triggered()), this, SLOT(commentClause()));

    aboutAct = new QAction(tr("&About"), this);
    aboutAct->setStatusTip(tr("Show the application's About box"));
    connect(aboutAct, SIGNAL(triggered()), this, SLOT(about()));

    aboutQtAct = new QAction(tr("About &Qt"), this);
    aboutQtAct->setStatusTip(tr("Show the Qt library's About box"));
    connect(aboutQtAct, SIGNAL(triggered()), qApp, SLOT(aboutQt()));

    // debugger

    makeAct = new QAction(png("wmaker_apps"), tr("&Make"), this);
    makeAct->setShortcut(QKeySequence::fromString("Ctrl+M"));
    makeAct->setStatusTip(tr("Save all modified files and call make"));
    makeAct->setIconVisibleInMenu(true);
    connect(makeAct, SIGNAL(triggered()), this, SLOT(make()));

    consultAct = new QAction(png("document-import-2"), tr("&Consult"), this);
    consultAct->setShortcut(QKeySequence::fromString("Ctrl+F5"));
    consultAct->setStatusTip(tr("Consult current saved file"));
    consultAct->setIconVisibleInMenu(true);
    connect(consultAct, SIGNAL(triggered()), this, SLOT(consult()));

    runAct = new QAction(png("run"), tr("&Run"), this);
    runAct->setShortcut(QKeySequence::fromString("F5"));
    runAct->setStatusTip(tr("Run current goal to completion"));
    runAct->setIconVisibleInMenu(true);
    connect(runAct, SIGNAL(triggered()), this, SLOT(run()));

    stopAct = new QAction(png("process-stop-6"), tr("&Stop (break)"), this);
    stopAct->setShortcut(QKeySequence::fromString("Shift+F5"));
    stopAct->setStatusTip(tr("Interrupt current goal"));
    stopAct->setIconVisibleInMenu(true);
    connect(stopAct, SIGNAL(triggered()), this, SLOT(stop()));

    stepInAct = new QAction(png("debug-step-into"), tr("Step &In (creep)"), this);
    stepInAct->setShortcut(QKeySequence::fromString("F11"));
    stepInAct->setStatusTip(tr("Run current goal"));
    stepInAct->setIconVisibleInMenu(true);
    connect(stepInAct, SIGNAL(triggered()), this, SLOT(stepIn()));

    stepOutAct = new QAction(png("debug-step-out"), tr("Step &Out"), this);
    stepOutAct->setShortcut(QKeySequence::fromString("Shift+F11"));
    stepOutAct->setStatusTip(tr("Run current goal to caller"));
    stepOutAct->setIconVisibleInMenu(true);
    connect(stepOutAct, SIGNAL(triggered()), this, SLOT(stepOut()));

    stepOverAct = new QAction(png("debug-step-over"), tr("Step Over (&leap)"), this);
    stepOverAct->setShortcut(QKeySequence::fromString("F10"));
    stepOverAct->setStatusTip(tr("Run current goal to termination"));
    stepOverAct->setIconVisibleInMenu(true);
    connect(stepOverAct, SIGNAL(triggered()), this, SLOT(stepOver()));

    toggleBPAct = new QAction(png("media-record-2"), tr("&Toggle Spy"), this);
    toggleBPAct->setShortcut(QKeySequence::fromString("F9"));
    toggleBPAct->setStatusTip(tr("Toggle Spy on current symbol"));
    toggleBPAct->setIconVisibleInMenu(true);
    connect(toggleBPAct, SIGNAL(triggered()), this, SLOT(toggleBP()));

    watchBPAct = new QAction(png("zoom-3"), tr("Var &Watch"), this);
    watchBPAct->setShortcut(QKeySequence::fromString("Shift+F9"));
    watchBPAct->setStatusTip(tr("Add/remove variable to Watch set"));
    watchBPAct->setIconVisibleInMenu(true);
    connect(watchBPAct, SIGNAL(triggered()), this, SLOT(watchVar()));
}

void MdiHelper::createMenus() {
    fileMenu = menuBar()->addMenu(tr("&File"));
    fileMenu->addAction(newAct);
    fileMenu->addAction(openAct);
    fileMenu->addAction(saveAct);
    fileMenu->addAction(saveAsAct);

    fileMenu->addSeparator();
    mruMenu = fileMenu->addMenu("Recent &Files...");

    //fileMenu->addSeparator();
    //fileMenu->addAction(switchLayoutDirectionAct);
    //fileMenu->addAction(switchViewModeAct);
    fileMenu->addSeparator();
    fileMenu->addAction(exitAct);

    editMenu = menuBar()->addMenu(tr("&Edit"));
    editMenu->addAction(cutAct);
    editMenu->addAction(copyAct);
    editMenu->addAction(pasteAct);
    editMenu->addSeparator();
    editMenu->addAction(findAct);
    editMenu->addAction(findNextAct);
    editMenu->addAction(findPreviousAct);
    editMenu->addAction(replaceAct);

    prefMenu = menuBar()->addMenu(tr("&Preferences"));
    prefMenu->addAction(viewSWIPrologPrefAct);
    prefMenu->addSeparator();
    prefMenu->addAction(selectColorsAct);
    prefMenu->addAction(selectFontAct);
    prefMenu->addAction(incFontAct);
    prefMenu->addAction(decFontAct);
    prefMenu->addSeparator();
    prefMenu->addAction(switchLayoutDirectionAct);
    prefMenu->addAction(switchViewModeAct);
/*
    debugMenu = menuBar()->addMenu(tr("&Debug"));
    debugMenu->addAction(makeAct);
    debugMenu->addAction(consultAct);
    debugMenu->addAction(runAct);
    debugMenu->addSeparator();
    debugMenu->addAction(watchBPAct);
    debugMenu->addSeparator();
    debugMenu->addAction(stopAct);
    debugMenu->addAction(stepInAct);
    debugMenu->addAction(stepOutAct);
    debugMenu->addAction(stepOverAct);
    debugMenu->addAction(toggleBPAct);
*/
    windowMenu = menuBar()->addMenu(tr("&Window"));
    updateWindowMenu();
    connect(windowMenu, SIGNAL(aboutToShow()), this, SLOT(updateWindowMenu()));

    menuBar()->addSeparator();

    helpMenu = menuBar()->addMenu(tr("&Help"));
    helpMenu->addAction(helpStartAct);
    helpMenu->addAction(helpDocAct);
    helpMenu->addSeparator();
    //helpMenu->addAction(viewCallGraphAct);
    helpMenu->addAction(viewGraphAct);
    helpMenu->addAction(viewGraphIncl);
    helpMenu->addAction(commentClauseAct);
    helpMenu->addSeparator();
    helpMenu->addAction(aboutAct);
    helpMenu->addAction(aboutQtAct);
}

void MdiHelper::createToolBars() {
    fileToolBar = addToolBar(tr("File"));
    fileToolBar->addAction(newAct);
    fileToolBar->addAction(openAct);
    fileToolBar->addAction(saveAct);

    editToolBar = addToolBar(tr("Edit"));
    editToolBar->addAction(cutAct);
    editToolBar->addAction(copyAct);
    editToolBar->addAction(pasteAct);
    editToolBar->addAction(findAct);
    editToolBar->addAction(replaceAct);
/*
    debugToolBar = addToolBar(tr("Debug"));
    debugToolBar->addAction(makeAct);
    debugToolBar->addAction(consultAct);
    debugToolBar->addAction(runAct);
    debugToolBar->addSeparator();
    debugToolBar->addAction(watchBPAct);
    debugToolBar->addSeparator();
    debugToolBar->addAction(stopAct);
    debugToolBar->addAction(stepInAct);
    debugToolBar->addAction(stepOutAct);
    debugToolBar->addAction(stepOverAct);
    debugToolBar->addAction(toggleBPAct);

    queriesBox = new QComboBox;
    queriesBox->setMinimumContentsLength(40);
    queriesBox->setToolTip(tr("Hold the query used to start the debugger"));
    queriesBox->setStatusTip(tr("Debugger will start with this query. <editor> refers to 'scriptname'."));
    debugToolBar->addWidget(queriesBox);
*/
    helpToolBar = addToolBar(tr("Help"));
}

void MdiHelper::createStatusBar() {
    statusBar()->showMessage(tr("Ready"));
    cursorIndicator.initializeStatusBar(statusBar());
}

void MdiHelper::subWindowActivated(QMdiSubWindow *w) {
    if (w) {
        qDebug() << "subWindowActivated" << w->widget()->metaObject()->className() << w->windowTitle();
        setWindowTitle(w->windowTitle());
        cursorIndicator.showCursorPosition(w->widget());
    }
}

void MdiHelper::cursorPositionChanged() {
    cursorIndicator.showCursorPosition(sender());
}

void MdiHelper::cut() {
    qDebug() << "cut";
}
void MdiHelper::copy() {
    qDebug() << "copy";
}
void MdiHelper::paste() {
    qDebug() << "paste";
}

void MdiHelper::switchLayoutDirection() {
    if (layoutDirection() == Qt::LeftToRight)
        qApp->setLayoutDirection(Qt::RightToLeft);
    else
        qApp->setLayoutDirection(Qt::LeftToRight);
}

void MdiHelper::switchViewMode() {
    if (mdiArea()->viewMode() == QMdiArea::SubWindowView)
        mdiArea()->setViewMode(QMdiArea::TabbedView);
    else
        mdiArea()->setViewMode(QMdiArea::SubWindowView);
}

void MdiHelper::setActiveSubWindow(QWidget *window) {
    QMdiSubWindow* sub = qobject_cast<QMdiSubWindow *>(window);
    qDebug() << "setActiveSubWindow" << sub;
    if (sub)
        mdiArea()->setActiveSubWindow(sub);
}
