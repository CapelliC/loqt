/*
    lqXDot_test   : interfacing Qt and Graphviz library

    Author        : Carlo Capelli
    E-mail        : cc.carlo.cap@gmail.com
    Copyright @ 2023

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

#include "mainwindow.h"
#include "lqPreferences.h"
#include "lqGvSynCol.h"
#include "ParenMatching.h"
#include "blockSig.h"
#include "XmlSyntaxHighlighter.h"
#include "file2string.h"

#include <QMenu>
#include <QMenuBar>
#include <QSettings>
#include <QFileDialog>
#include <QTemporaryFile>
#include <QDateTime>
#include <QApplication>
#include <QTextStream>
#include <QDebug>

/** GUI setup
 */
MainWindow::MainWindow(int argc, char *argv[], QWidget *parent)
    : QMainWindow(parent), MruHelper("dots")
{
    rci.initializeStatusBar(statusBar());

    lqPreferences p;
    p.loadGeometry(this);

    fileDot = p.value("fileDot").toString();
    lastDir = p.value("lastDir").toString();
    lastMode = p.value("lastMode", "dot").toString();

    QMenu *m = menuBar()->addMenu("&File");
    m->addAction(tr("&New..."), QKeySequence::New, this, &MainWindow::newFile);
    m->addAction(tr("Open..."), QKeySequence::Open, this, &MainWindow::openFile);
    m->addMenu(mruMenu = new QMenu("Recent &Files..."));
    loadMru(p, this);
    m->addSeparator();
    m->addAction(tr("&Save"), QKeySequence::Save, this, &MainWindow::saveFile);
    m->addAction(tr("Save &As..."), QKeySequence::SaveAs, this, &MainWindow::saveFileAs);
    m->addAction(tr("&Render..."), tr("Ctrl+R"), this, &MainWindow::renderFile);

    // layouts algorithm by name
    m->addSeparator();

    QString mode = lastMode;
    if (argc == 3)
        mode = argv[2];

    foreach (QString layout, QString("dot neato fdp sfdp twopi circo").split(' ')) {
        auto a = m->addAction(layout, this, SLOT(changeLayout()));
        a->setCheckable(true);
        a->setChecked(layout == mode);
    }

    m->addSeparator();
    m->addAction(tr("E&xit"), QKeySequence::Quit, qApp, SLOT(quit()));

    if (argc >= 2)
        fileDot = argv[1];

    if (fileDot.length())
        try {
            viewDot();
        } catch(const std::exception& ex)  {
            errbox(ex.what(), QObject::tr("exception"));
        }

    connect(&monitorScript, SIGNAL(fileChanged(QString)), this, SLOT(scriptChanged(QString)));

    qApp->setWindowIcon(QIcon(":/app.png"));
}

/** save settings
 */
MainWindow::~MainWindow() {
    lqPreferences p;
    p.saveGeometry(this);
    p.setValue("fileDot", fileDot);
    p.setValue("lastDir", lastDir);
    p.setValue("lastMode", lastMode);
    storeMru(p);
    p.save();

    delete gvsyn;
}

/** handle details on application quit
 */
void MainWindow::closeEvent(QCloseEvent *e) {
    if (isWindowModified()) {
        QMessageBox box(this);
        box.setIcon(box.Warning);
        box.setInformativeText(tr("the current script has been modified!"));
        box.setText(tr("do you want to save your changes ?"));
        box.setStandardButtons(box.Save|box.Discard|box.Cancel);
        box.setDefaultButton(box.Save);
        switch (box.exec()) {
        case box.Save:
            saveFile();
            break;
        case box.Discard:
            break;
        default: //box.Cancel:
            e->ignore();
        }
    }
}

inline QString dotFiles() { return QObject::tr("Dot Scripts (*.gv *.dot)"); }

/** run selection dialog and run selected file
 */
void MainWindow::openFile() {
    QFileDialog d(this, tr("Open Dot Script"), lastDir, dotFiles());
    if (d.exec()) {
        lastDir = d.directory().path();
        fileDot = d.selectedFiles().constFirst();
        viewDot();
    }
}

/** save file, report errors
 */
void MainWindow::saveViewDot(QString file, QString script, QString errmsg) {
    QFile f(file);
    monitorScript.removePath(file);
    if (f.open(f.WriteOnly|f.Text)) {
        f.write(script.toUtf8());
        f.close();
        fileDot = file;
        viewDot();
        setWindowModified(false);
    }
    else
        errbox(tr("error writing %1").arg(file), errmsg);
}

/** make backup and save script file
 */
void MainWindow::saveFile() {
    QString fileBak = fileDot + ".bak";
    QFile::remove(fileBak);
    if (!QFile::copy(fileDot, fileBak))
        errbox(tr("error writing %1").arg(fileBak), tr("cannot make backup copy"));

    saveViewDot(fileDot, source()->toPlainText(), tr("cannot save Dot Script"));
}

/** render current graph as SVG, in a temporary file name
 */
void MainWindow::makeSvg(QString f) {
    if (*view()) {
        QString name;
        if (f.isEmpty())
            name = QString("%1/XXXXXX.svg").arg(lastDir);
        else {
            QFileInfo i(f);
            name = QString("%1/%2.svg").arg(i.absoluteDir().canonicalPath(), i.baseName());
        }
        qDebug() << name;
        if (gvRenderFilename(view()->getContext(), view()->getGraph(), "svg", name.toUtf8().constData()) == 0) {
            svgv()->openFile(name);
            svgxml()->setPlainText(file2string(name));
            new XmlSyntaxHighlighter(svgxml()->document());
        }
        else
            errbox(tr("cannot generate SVG file"), tr("error opening '%1'").arg(name));
    }
}

/** get name and make a new file of it
 */
void MainWindow::saveFileAs() {
    QFileDialog fd(this, tr("Save Dot Script As"), lastDir, dotFiles());
    fd.setAcceptMode(fd.AcceptSave);
    fd.setDefaultSuffix("gv");
    if (fd.exec())
        saveViewDot(fd.selectedFiles().constFirst(), source()->toPlainText(), tr("cannot save Dot Script"));
}

/** refresh graph display from (possibly) dirty script
 */
void MainWindow::renderFile() {
    if (view()->render_script(source()->toPlainText(), lastMode)) {
        makeSvg(QString());
        tabs->setCurrentIndex(t_dot);
    }
}

/** create a new dot script
 */
void MainWindow::newFile() {
    QFileDialog fd(this, tr("New Dot Script"), lastDir, dotFiles());
    fd.setAcceptMode(fd.AcceptSave);
    fd.setDefaultSuffix("gv");
    if (fd.exec()) {
        QString newPath = fd.selectedFiles().constFirst();
        saveViewDot(newPath,
                    tr("/* script file %1\n"
                       " * created at %2\n"
                       " */\n"
                       "digraph %3 {\n"
                       "}\n"
                    ).arg(newPath, QDateTime::currentDateTime().toString(), QFileInfo(newPath).baseName().replace(' ', '_')),
                    tr("cannot create new dot script"));
    }
}

/** this is default as required by MruHelper
 */
void MainWindow::openFileIndex(int i) {
    fileDot = files[i];
    viewDot();
}

/** reinitialize GUI with required script
 */
void MainWindow::viewDot() {
    QString f = fileDot;
    delete tabs;

    QString layout;
    foreach (QAction *a, menuBar()->actions()[0]->menu()->actions())
        if (a->isChecked())
            layout = a->text();
    Q_ASSERT(!layout.isEmpty());

    setWindowTitle(QString("%1[*]").arg(f));

    QFile t(f);
    if (t.open(t.ReadOnly|t.Text)) {
        setCentralWidget(tabs = new QTabWidget);

        auto s1 = new QSplitter();
        s1->addWidget(new lqXDotView);
        s1->addWidget(new QTextEdit);
        tabs->addTab(s1, tr("&Dot view/source"));

        auto s2 = new QSplitter();
        s2->addWidget(new SvgView);
        s2->addWidget(new QTextEdit);
        tabs->addTab(s2, tr("&Svg view/source"));
        /*
        tabs->addTab(new lqXDotView,    tr("lq&XDot view"));
        tabs->addTab(new SvgView,       tr("&Svg view"));
        tabs->addTab(new QTextEdit,     tr("&Dot source"));
        tabs->addTab(new QTextEdit,     tr("Svg Xml sourc&e"));
        */

        lqPreferences p;
        //source()->setFont(p.console_font);
        source()->setLineWrapMode(QTextEdit::NoWrap);

        //svgxml()->setFont(p.console_font);
        svgxml()->setLineWrapMode(QTextEdit::NoWrap);

        source()->setPlainText(file2string(t));

        connect(source(), SIGNAL(textChanged()), this, SLOT(textChanged()));
        connect(source(), SIGNAL(cursorPositionChanged()), this, SLOT(cursorPositionChanged()));

        // syntax coloring
        mode = highlighting;

        delete gvsyn;
        gvsyn = new lqGvSynCol(source());

        if (view()->render_file(f, lastMode = layout))
            makeSvg(f);

        // keep MRU updated
        insertPath(this, f);

        // get notified on file changes
        monitorScript.addPath(fileDot);
    }
    else {
        errbox(tr("Cannot read %1").arg(f), tr("open file failed"));
        removePath(this, f);
    }
}

/** change the rendered
 */
void MainWindow::changeLayout() {
    foreach (QAction *a, menuBar()->actions()[0]->menu()->actions())
        if (a->isCheckable())
            a->setChecked(false);
    qobject_cast<QAction*>(sender())->setChecked(true);
    viewDot();
}

/** keep modified status updated
 */
void MainWindow::textChanged() {
    if (mode == highlighting)
        mode = editing;
    else
        setWindowModified(true);
}

/** display infomation about cursor
 */
void MainWindow::cursorPositionChanged() {

    QTextCursor c = source()->textCursor();

    if (paren.size()) {
        blockSig bs(source()->document());
        paren.format_both(c);
        paren = range();
    }

    ParenMatching pm(c);
    if (pm) {
        blockSig bs(source()->document());
        (paren = pm.positions).format_both(c, paren.bold());
    }

    rci.showCursorPosition(source());
}

void MainWindow::scriptChanged(QString path) {
    MB req(MB::Warning, tr("Warning"), tr("Script has been modified"), MB::Yes|MB::Ignore, this);
    req.setInformativeText(tr("Do you want to reload '%1'").arg(path));
    if (req.exec() == MB::Yes)
        viewDot();
}

/** factorize common usage
 */
QMessageBox::StandardButton MainWindow::errbox(QString msg, QString info, MB::StandardButtons buttons, MB::Icon ic, QString title) {
    if (title.isEmpty())
        title = tr("Error");
    MB box(ic, title, msg, buttons, this);
    box.setInformativeText(info);
    return MB::StandardButton(box.exec());
}
