/*
    lqUty        : loqt utilities

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

#include "SourceEdit.h"
#include "file2string.h"
#include "MruHelper.h"

#include <QWebFrame>
#include <QWebInspector>
#include <QTextStream>
#include <QFile>
#include <QMessageBox>
#include <QApplication>
#include <QFileDialog>
#include <QCloseEvent>
#include <QtDebug>

void SourceEdit::initialize() {
    status = idle;
    connect(this, SIGNAL(loadFinished(bool)), SLOT(loadFinished(bool)));
}

SourceEdit::SourceEdit() { initialize(); }

SourceEdit::SourceEdit(QString file) {
    initialize();
    loadFile(file);
}

SourceEdit::SourceEdit(const SourceEdit &e) : QWebView() {
    initialize();
    loadFile(file);
}

QString SourceEdit::symbol() const { return QFileInfo(file).baseName(); }

void SourceEdit::newFile() {
}

bool SourceEdit::loadFile(QString fileName) {
    file = fileName;
    text = file2string(fileName);
    setHtml(file2string(":/SourceEdit.html"), QUrl("qrc:/"));
    return true;
}
QString SourceEdit::toPlainText() const {
    return page()->mainFrame()->evaluateJavaScript("editor.getValue()").toString();
}

void SourceEdit::loadFinished(bool ok) {
    emit msg(log, QString("loadFinished %1, len %2, ok %3").arg(file).arg(text.length()).arg(ok));
    if (ok) {
        auto f = page()->mainFrame();
        f->addToJavaScriptWindowObject("proxy", this);
        f->evaluateJavaScript("editor.setValue(proxy.data)");//.toString();
        f->evaluateJavaScript("editor.on(\"change\", function() { proxy.onChange() })");
    }
}

bool SourceEdit::save() {
    {   QFile f(file);
        if (!f.open(QFile::WriteOnly)) {
            emit msg(err, tr("Can't open %1 for writing!").arg(file));
            return false;
        }
        f.write(toPlainText().toUtf8());
    }

    status = saved;
    emit setTitle(file, title());
    emit msg(info, tr("Saved '%1'").arg(symbol()));

    return true;
}

bool SourceEdit::saveAs() {
    /*
    QString fileName =
        QFileDialog::getSaveFileName(this,
            tr("Save As"), curFile);
    if (fileName.isEmpty())
        return false;

    return saveFile(fileName);
    */
    return true;
}

void SourceEdit::closeEvent(QCloseEvent *event) {
    if (maybeSave()) {
        event->accept();
    } else {
        event->ignore();
    }
}

void SourceEdit::documentWasModified() {
    //setWindowModified(document()->isModified());
}

QString SourceEdit::title() const {
    QString t = MruHelper::path2title(file);
    return status == modified ? t + " * " : t;
}

bool SourceEdit::maybeSave() {
    if (status == modified) {
        typedef QMessageBox B;
        B::StandardButton ret = B::warning(this,
           tr("Source %1").arg(symbol()),
           tr("'%1' has been modified.\n"
              "Do you want to save your changes?").arg(file), B::Save|B::Discard|B::Cancel);
        if (ret == B::Save)
            return save();
        else if (ret == B::Cancel)
            return false;
    }
    return true;
}

// callback from JS
void SourceEdit::onChange() {
    if (status != modified) {
        status = modified;
        emit setTitle(file, title());
    }
}

void SourceEdit::show_call(long from, long stop) {
    page()->mainFrame()->evaluateJavaScript(
                QString("show_call(%1,%2)").arg(from).arg(stop));
}
