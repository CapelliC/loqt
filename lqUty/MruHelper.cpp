/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
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

#include "MruHelper.h"
#include <QDebug>

/** keep QSettings keys with reasonable defaults
 */
MruHelper::MruHelper(QString key, QString entry)
    : mruMenu(0), key(key), entry(entry), cmdMapper(new QSignalMapper)
{
}

/** load the menu and initialize actions
 */
void MruHelper::loadMru(QSettings &s, QObject *parent, const char *slot) {
    QObject::connect(cmdMapper, SIGNAL(mapped(int)), parent, slot);
    files = s.value(key).toStringList();
    max_files = s.value("max_files_" + key, 16).toInt();
    fillMru(parent);
}

/** write menu entries to QSettings storage
 */
void MruHelper::storeMru(QSettings &s) {
    s.setValue(key, files);
}

/** keep <path> in MRU list
 */
bool MruHelper::insertPath(QObject* parent, QString path) {
    int ix = files.indexOf(path);
    if (ix != -1) {
        if (ix > 0) {
            files.move(ix, 0);
            fillMru(parent);
        }
        return false;
    }
    files.insert(0, path);
    fillMru(parent);
    return true;
}

/** remove <path> - if found - from MRU list
 */
bool MruHelper::removePath(QObject *parent, QString path) {
    if (files.removeOne(path)) {
        fillMru(parent);
        return true;
    }
    return false;
}

/** setup MRU menu list
 */
void MruHelper::fillMru(QObject* parent) {
    while (files.size() > max_files)
        files.removeLast();
    mruMenu->clear();
    for (int i = 0; i < files.size(); ++i) {
        QString path = files[i],
                title = path2title(path),
                entry = i < 9 ? QString("&%1 %2").arg(i + 1).arg(title) : title;
        QAction *a = new QAction(entry, parent);
        a->setStatusTip(path);
        cmdMapper->setMapping(a, i);
        a->connect(a, SIGNAL(triggered()), cmdMapper, SLOT(map()));
        mruMenu->addAction(a);
    }
}
