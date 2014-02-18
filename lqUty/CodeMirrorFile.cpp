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

#include "CodeMirrorFile.h"
#include "file2string.h"

#include <QFileInfo>
#include <QMessageBox>
#include <stdexcept>

CodeMirrorFile::CodeMirrorFile(QWidget *parent) : CodeMirror(parent), makeBackup_(true)
{
}
CodeMirrorFile::CodeMirrorFile(const CodeMirrorFile &copy) : CodeMirror()
{
    path_ = copy.path_;
    makeBackup_ = copy.makeBackup_;
}

/** load <fileName> to textarea with error check
 */
bool CodeMirrorFile::loadFile(QString fileName) {
    try {
        path_ = fileName;
        //setPlainText(file2string(fileName));  TBD put a tab at strat document - not good
        text = file2string(fileName);
        setHtml(file2string(":/CodeMirror.html"), QUrl("qrc:/"));
    }
    catch(std::exception& e) {
        return error(e.what());
    }
    return true;
}

/** make backup and save script file
*/
bool CodeMirrorFile::saveFile()
{
    if (makeBackup_) {
        QString bak = path_ + ".bak";
        QFile::remove(bak);
        if (QFile::exists(path_) && !QFile::copy(path_, bak))
            return error(tr("cannot make backup '%1'").arg(bak));
    }

    QFile f(path_);
    if (!f.open(f.WriteOnly|f.Text) || f.write(toPlainText().toUtf8()) == -1)
        return error(tr("cannot write to '%1'").arg(path_));

    return true;
}

/** report message and return false
 */
bool CodeMirrorFile::error(QString msg)
{
    emit userMessage(err, msg);
    return false;
}
