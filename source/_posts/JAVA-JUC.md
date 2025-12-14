---
title: JAVA-JUC
date: 2025-12-14 15:23:48
description: JAVA-JUC 并发
tags:
    - JAVA
    - JUC
categories:
    - JAVA
top_img: false
cover: /image/post_cover/java-juc-concurrency-min.svg
---

## A-001：并发

有效利用每一份资源，精准控制每一次访问

### B-001：Synchronized

（1）本质：引入上级：即当前级无法有效区分，引入上级控制

（2）用法：锁对象、锁类

（3）变量安全分析：成员变量、静态变量、局部变量

1. 使用合适的方法修饰符，避免子类重写父类，使得局部变量可能出现并发问题（闭原则）

（4）线程安全类：String、Integer、StringBuffer、Random、Vector、Hashtable、java.util.concurrent

1. 多个线程
