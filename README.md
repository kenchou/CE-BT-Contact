CE-BT-Contact
=============
Contact import tools for BYDAuto (WinCE)

vCard Tools
-----------
vcf2csv 抽取 vCard 名片文件(*.vcf) 生成 contact.csv，生成的 csv 可编辑后导入比亚迪车载系统的蓝牙通讯录。

### 使用方法
#### 1. 导出 vCard
##### iOS 设备（iPhone, iPad 等）:

访问 https://www.icloud.com/#contacts
选择要导出的联系人，点击左下角 `齿轮` 图标，选择 `导出 vCard...`

注意：
   由于 iCloud 的 bug，只有 Safari 或者 Firefox 点击导出才会有反应。
   IE 用户： 将 icloud.com 加入兼容性视图，然后 `按住 Shift 点击` 或者 `右键` 点击 `导出 vCard...`。
   Chrome 用户: 右键点击 `导出 vCard...`

##### Android 设备

联系人直接导出即可


#### 2. 在电脑上转换 vCard 到 contact.csv

##### Windows 用户

将 *.vcf 文件拖到 vcf2csv.exe 上即可。
或者在命令行中执行（file1.vcf 用实际文件名取代， 多个文件用空格隔开，文件名含有空格的用引号包起来）：
~~~
vcf2csv file1.vcf
~~~

##### Mac/Linux 用户
运行 python 版本的 vcf2csv

~~~bash
./vcf2csv.py file1.vcf
~~~

或者通过管道

~~~bash
cat *.vcf | ./vcf2csv.py
~~~

### 3. 导入比亚迪车载系统
将上面生成的 contact.csv 复制到 TF 卡，放置在导入程序同一目录下，上车操作。
