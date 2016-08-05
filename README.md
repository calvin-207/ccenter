# 中央机
---------

## 功能

应用层实现的一套节点连接的信息，节点加入、节点退出、节点通讯。主要是为了解决节点不适用erlang:register({global, Name} Dest) 带来的节点信息不透明的问题。

1. 节点加入的功能回调
2. 节点退出的功能回调
3. 节点监控
4. 垮节点PID的存储
5. 垮节点通讯接口

## 逻辑

根据参数 type 指定类型为normal或者center， 如果是center则启动：
    1. center_server：负责监控节点的接入和退出，启动center_connect_server进程
    2. center_connect_sup：负责监控ceter_connect_server进程池
    3. center_connect_server：与节点之间的一对一服务进程，主要用来处理中央府广播消息给全部节点的需求

如果type是normal则简化成：
    1. normal_connect_server：负责与中央府连接，监控中央府并保存中央服相关的信息。

## 使用

1. 配置
ccenter.app.src 下 配置 type 变量
2. 启动
application:start(ccenter)
3. 中央服获取节点连如信息：
ccenter:games()

其他接口请参考 ccenter.erl模块


