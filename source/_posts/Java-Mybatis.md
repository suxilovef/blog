---
title: Java-Mybatis
date: 2025-12-15 21:34:12
description: Java-Mybatis 框架
tags:
  - Java
  - Mybatis
categories:
  - Java
top_img: false
cover: /image/post_cover/java-mybatis-min2.svg
---

# Mybatis

## I：前言

版本：`<mybatis-plus.version>3.4.1</mybatis-plus.version>`

定位：协助完全控制SQL，抽取封装模板代码。

分析原则：度量取舍

备注：截取关键源码分析

## II：分析

### A-00X：全局配置文件解析

只分析从xml配置文件解析，其它路径殊途同归，暂不分析

#### **SqlSessionFactoryBuilder**

```java
String resource = "org/apache/ibatis/test/mybatis.xml";
InputStream inputStream = Resources.getResourceAsStream(resource);
SqlSessionFactory sqlSessionFactory = new SqlSessionFactoryBuilder().build(inputStream);
```

#### **XMLConfigBuilder**

```java
XMLConfigBuilder parser = new XMLConfigBuilder(reader, environment, properties);
//....
public XMLConfigBuilder(InputStream inputStream, String environment, Properties props) {
    this(new XPathParser(inputStream, true, props, new XMLMapperEntityResolver()), environment, props);
}
//....->Document
// 扩展：DOM解析器、SAX解析器
public XPathParser(InputStream inputStream, boolean validation, Properties variables, EntityResolver entityResolver) {
    commonConstructor(validation, variables, entityResolver);
    this.document = createDocument(new InputSource(inputStream));
}
//Document->Configuration
// 扩展：Document->业务类转换方式：手动硬编码，JAXB注解，第三方工具Dom4j
private void parseConfiguration(XNode root) {
    try {
        // issue #117 read properties first
        // ....
        mapperElement(root.evalNode("mappers"));
    } catch (Exception e) {
        throw new BuilderException("Error parsing SQL Mapper Configuration. Cause: " + e, e);
    }
}
// Configuration其他属性的Doc映射：略
// mappers
// 简化源码：
private void mapperElement(XNode parent) throws Exception {
    if ("package".equals(child.getName())) {
        String mapperPackage = child.getStringAttribute("name");
        // mapper接口解析
        configuration.addMappers(mapperPackage);
    } else {
        ErrorContext.instance().resource(resource);
        InputStream inputStream = Resources.getResourceAsStream(resource);
        // mapperXML文件解析
        XMLMapperBuilder mapperParser = new XMLMapperBuilder(inputStream, configuration, resource, configuration.getSqlFragments());
    }
}
// mapper接口解析：目标Configuration持有mapper接口相关信息：
mapperRegistry.addMappers(packageName);
// MapperRegistry
private final Map<Class<?>, MapperProxyFactory<?>> knownMappers = new HashMap<>();
// 解析mapper接口
MapperAnnotationBuilder parser = new MapperAnnotationBuilder(config, type);
parser.parse();
```

#### **MapperAnnotationBuilder**

```java
public MapperAnnotationBuilder(Configuration configuration, Class<?> type) {
    String resource = type.getName().replace('.', '/') + ".java (best guess)";
    // mapper构建助手
    this.assistant = new MapperBuilderAssistant(configuration, resource);
    this.configuration = configuration;
    this.type = type;
}
// 解析
public void parse() {
    String resource = type.toString();
    if (!configuration.isResourceLoaded(resource)) {
        // 解析mapper映射文件 TODO：Configuration是怎么维护Mapper接口与MapperXML文件的
        // XMLMapperBuilder xmlParser = new XMLMapperBuilder(....);
        // xmlParser.parse();
        loadXmlResource();
        configuration.addLoadedResource(resource);
        assistant.setCurrentNamespace(type.getName());
        // 解析缓存配置
        parseCache();
        parseCacheRef();
        for (Method method : type.getMethods()) {
            if (!canHaveStatement(method)) {
                continue;
            }
            if (getAnnotationWrapper(method, false, Select.class, SelectProvider.class).isPresent()
                && method.getAnnotation(ResultMap.class) == null) {
                parseResultMap(method);
            }
            try {
                // mapper接口中的每个方法都解析成MappedStatement对象
                parseStatement(method);
            } catch (IncompleteElementException e) {
                configuration.addIncompleteMethod(new MethodResolver(this, method));
            }
        }
    }
    parsePendingMethods();
}
// 解析方法
// 把mapper接口中的一个方法（带有mybatis注解）解析为Mybatis内部的一个MappedStatement，并注册到Configuration中
void parseStatement(Method method) {
    // SQL参数类型
    final Class<?> parameterTypeClass = getParameterType(method);
    // TODO：待确认：数据库方言
    final LanguageDriver languageDriver = getLanguageDriver(method);

    getAnnotationWrapper(method, true, statementAnnotationTypes).ifPresent(statementAnnotation -> {
        // SqlSource
        final SqlSource sqlSource = buildSqlSource(statementAnnotation.getAnnotation(), parameterTypeClass, languageDriver, method);
        // SqlCommandType
        final SqlCommandType sqlCommandType = statementAnnotation.getSqlCommandType();
        // 取方法上的@Option注解，用来覆写默认设置
        final Options options = getAnnotationWrapper(method, false, Options.class).map(x -> (Options)x.getAnnotation()).orElse(null);
        final String mappedStatementId = type.getName() + "." + method.getName();

        // 处理主键生成策略
        // 优先检查@SelectKey注解：调用handleSelectKeyAnnotation创建一个新的MappedStatement
        // 用于执行SelectKey注解里的语句，再创建一个SelectKeyGenerator，并注册到Configuration
        final KeyGenerator keyGenerator;
        String keyProperty = null;
        String keyColumn = null;
        if (SqlCommandType.INSERT.equals(sqlCommandType) || SqlCommandType.UPDATE.equals(sqlCommandType)) {
            // first check for SelectKey annotation - that overrides everything else
            SelectKey selectKey = getAnnotationWrapper(method, false, SelectKey.class).map(x -> (SelectKey)x.getAnnotation()).orElse(null);
            if (selectKey != null) {
                keyGenerator = handleSelectKeyAnnotation(selectKey, mappedStatementId, getParameterType(method), languageDriver);
                keyProperty = selectKey.keyProperty();
            } else if (options == null) {
                // 全局配置
                keyGenerator = configuration.isUseGeneratedKeys() ? Jdbc3KeyGenerator.INSTANCE : NoKeyGenerator.INSTANCE;
            } else {
                // options 选择KeyGenerator
                keyGenerator = options.useGeneratedKeys() ? Jdbc3KeyGenerator.INSTANCE : NoKeyGenerator.INSTANCE;
                keyProperty = options.keyProperty();
                keyColumn = options.keyColumn();
            }
        } else {
            // 非INSERT/UPDATE 统一使用NoKeyGenerator.INSTANCE：没有主键生成逻辑
            keyGenerator = NoKeyGenerator.INSTANCE;
        }

        Integer fetchSize = null;
        Integer timeout = null;
        StatementType statementType = StatementType.PREPARED;
        ResultSetType resultSetType = configuration.getDefaultResultSetType();
        // 非SELECT操作，默认刷新二级缓存
        boolean isSelect = sqlCommandType == SqlCommandType.SELECT;
        boolean flushCache = !isSelect;
        boolean useCache = isSelect;
        if (options != null) {
            if (FlushCachePolicy.TRUE.equals(options.flushCache())) {
                flushCache = true;
            } else if (FlushCachePolicy.FALSE.equals(options.flushCache())) {
                flushCache = false;
            }
            useCache = options.useCache();
            fetchSize = options.fetchSize() > -1 || options.fetchSize() == Integer.MIN_VALUE ? options.fetchSize() : null; //issue #348
            timeout = options.timeout() > -1 ? options.timeout() : null;
            statementType = options.statementType();
            if (options.resultSetType() != ResultSetType.DEFAULT) {
                resultSetType = options.resultSetType();
            }
        }
        // TODO：待确认：这里有瑕疵：@ResultMap在上一个方法被解析，但应该放在这里解析的
        String resultMapId = null;
        if (isSelect) {
            ResultMap resultMapAnnotation = method.getAnnotation(ResultMap.class);
            if (resultMapAnnotation != null) {
                resultMapId = String.join(",", resultMapAnnotation.value());
            } else {
                resultMapId = generateResultMapName(method);
            }
        }

        assistant.addMappedStatement(
            mappedStatementId,
            sqlSource,
            statementType,
            sqlCommandType,
            fetchSize,
            timeout,
            // ParameterMapID
            null,
            parameterTypeClass,
            resultMapId,
            getReturnType(method),
            resultSetType,
            flushCache,
            useCache,
            // TODO gcode issue #577
            false,
            keyGenerator,
            keyProperty,
            keyColumn,
            statementAnnotation.getDatabaseId(),
            languageDriver,
            // ResultSets
            options != null ? nullOrEmpty(options.resultSets()) : null);
    });
}
```

#### **MapperBuilderAssistant**

mapper构建助手：协助解析器完成mapperStatement的构建

#### **XMLMapperBuilder**

```java
// XML文件解析：准备解析环境
XMLMapperBuilder mapperParser = new XMLMapperBuilder(inputStream, configuration, resource, configuration.getSqlFragments());
// 开始解析
mapperParser.parse();

public XMLMapperBuilder(InputStream inputStream, Configuration configuration, String resource, Map<String, XNode> sqlFragments) {
    this(new XPathParser(inputStream, true, configuration.getVariables(), new XMLMapperEntityResolver()),
         configuration, resource, sqlFragments);
}

private XMLMapperBuilder(XPathParser parser, Configuration configuration, String resource, Map<String, XNode> sqlFragments) {
    super(configuration);
    // mapper构建助手，协助构建mapperStatement
    this.builderAssistant = new MapperBuilderAssistant(configuration, resource);
    this.parser = parser;
    this.sqlFragments = sqlFragments;
    this.resource = resource;
}

public void parse() {
    if (!configuration.isResourceLoaded(resource)) {
        // 从映射文件中的<mapper>根标签开始解析
        configurationElement(parser.evalNode("/mapper"));
        configuration.addLoadedResource(resource);
        bindMapperForNamespace();
    }

    parsePendingResultMaps();
    parsePendingCacheRefs();
    parsePendingStatements();
}
private void configurationElement(XNode context) {
    try {
        // 这里需要根据各标签之间的依赖关系按次序解析
        String namespace = context.getStringAttribute("namespace");
        if (namespace == null || namespace.isEmpty()) {
            throw new BuilderException("Mapper's namespace cannot be empty");
        }
        builderAssistant.setCurrentNamespace(namespace);
        cacheRefElement(context.evalNode("cache-ref"));
        cacheElement(context.evalNode("cache"));
        parameterMapElement(context.evalNodes("/mapper/parameterMap"));
        resultMapElements(context.evalNodes("/mapper/resultMap"));
        // TODO
        sqlElement(context.evalNodes("/mapper/sql"));
        // 解析成MapperStatement：解析过程与注解路线基本一致：暂不分析,只分析关键步骤
        buildStatementFromContext(context.evalNodes("select|insert|update|delete"));
    } catch (Exception e) {
        throw new BuilderException("Error parsing Mapper XML. The XML location is '" + resource + "'. Cause: " + e, e);
    }
}
```

#### **SqlSource**

Annotation、XML两条解析路线均涉及SqlSource解析

核心实现类：

`StaticSqlSource`：静态SQL（无动态标签、无${}占位符），直接存储最终可执行SQL

`DynamicSqlSource`：动态SQL（包含<if>等标签），运行时才拼接生成最终SQL

`RawSqlSource`：含${}占位符的SQL，初始化时解析${}，内部最终生成StaticSqlSource

`ProviderSqlSource`：注解专属（如@SelectProvider），运行时通过反射调用指定方法生成SQL

小结：

### A-00X：SQL执行

#### SqlSession

创建

```java
SqlSessionFactory sqlSessionFactory = new SqlSessionFactoryBuilder().build(inputStream);
SqlSession sqlSession = sqlSessionFactory.openSession();
```

```java
// DefaultSqlSessionFactory
private SqlSession openSessionFromDataSource(ExecutorType execType, TransactionIsolationLevel level, boolean autoCommit) {
    Transaction tx = null;
    try {
        final Environment environment = configuration.getEnvironment();
        final TransactionFactory transactionFactory = getTransactionFactoryFromEnvironment(environment);
        // JdbcTransaction ManagedTransaction SpringManagedTransaction
        tx = transactionFactory.newTransaction(environment.getDataSource(), level, autoCommit);
        // execType：三种类型：SIMPLE、REUSE、BATCH
        // Executor 持有Transaction
        final Executor executor = configuration.newExecutor(tx, execType);
        // sqlSession 持有Configuration、Executor
        return new DefaultSqlSession(configuration, executor, autoCommit);
    } catch (Exception e) {
        closeTransaction(tx); // may have fetched a connection so lets call close()
        throw ExceptionFactory.wrapException("Error opening session.  Cause: " + e, e);
    } finally {
        ErrorContext.instance().reset();
    }
}

public Executor newExecutor(Transaction transaction, ExecutorType executorType) {
    executorType = executorType == null ? defaultExecutorType : executorType;
    executorType = executorType == null ? ExecutorType.SIMPLE : executorType;
    Executor executor;
    if (ExecutorType.BATCH == executorType) {
        // update累积批处理：query前会先flush（BatchExecutor.doUpdate/doQuery）
        executor = new BatchExecutor(this, transaction);
    } else if (ExecutorType.REUSE == executorType) {
        // 按SQL复用Statement
        executor = new ReuseExecutor(this, transaction);
    } else {
        // 每次执行都新建Statement
        executor = new SimpleExecutor(this, transaction);
    }
    if (cacheEnabled) {
        // public CachingExecutor(Executor delegate) {
        // this.delegate = delegate;
        // delegate.setExecutorWrapper(this);
        // }
        executor = new CachingExecutor(executor);
    }
    // 插件链对Executor进行代理包装
    executor = (Executor) interceptorChain.pluginAll(executor);
    return executor;
}
```



#### **Executor**

抽象实现类：BaseExecutor

BaseExecutor子类：SimpleExecutor、BatchExecutor、ReuseExecutor、ClosedExecutor

创建：在SqlSession创建过程中创建

相关设计模式：

1. 模板方法加策略
   1. Executor是策略接口，提供了统一的执行能力抽象
   2. BaseExecutor提供了大量通用逻辑（本地缓存，query栈，延迟加载、事务生命周期等），并且定义了抽象的doUpdate、doQuery、doQueryCursor方法
   3. 具体JDBC行为由子类实现
   4. Configuration.newExecutor会根据ExecutorType创建具体策略
2. 装饰器模式
   1. CachingExecutor是对Executor的装饰器，在执行前后增加二级缓存逻辑
      1. 根据MappedStatement上是否配置<cache/>或useCache，决定是否使用二级缓存
      2. 使用TransactionalCacheManager包装Cache，实现事务级的一致性
   2. Configuration.newExecutor包装
3. 插件机制（JDK代理）
   1. 按加载顺序依次代理

```java
// 获取到mapper代理对象
AuthorMapper mapper = sqlSession.getMapper(AuthorMapper.class);
Author a = mapper.selectAuthor(101);

// 获取mapper代理对象
// DefaultSqlSession
@Override
public <T> T getMapper(Class<T> type) {
    return configuration.getMapper(type, this);
}
// Configuration
public <T> T getMapper(Class<T> type, SqlSession sqlSession) {
    return mapperRegistry.getMapper(type, sqlSession);
}
// MapperRegistry
public <T> T getMapper(Class<T> type, SqlSession sqlSession) {
    final MapperProxyFactory<T> mapperProxyFactory = (MapperProxyFactory<T>) knownMappers.get(type);
    if (mapperProxyFactory == null) {
        throw new BindingException("Type " + type + " is not known to the MapperRegistry.");
    }
    try {
        return mapperProxyFactory.newInstance(sqlSession);
    } catch (Exception e) {
        throw new BindingException("Error getting mapper instance. Cause: " + e, e);
    }
}
// TODO JDK动态代理：代理类根据SQL类型分发到sqlSession.selectOne/selectList/...执行
```

**DefaultSqlSession**

以 `selectList`为例分析

```

```



## III：使用

### A-00X：Mybatis注解开发

#### **实体类**

```java
public class User {
    private Long id;
    private String username;
    private String email;
}
```

#### **一、基础CRUD注解**

```java
public interface UserMapper {

    @Select("select id, username, email from user where id = #{id}")
    User selectById(Long id);

    @Select("select id, username, email from user")
    List<User> selectAll();

    @Insert("insert into user(username, email) values(#{username}, #{email})")
    int insert(User user);

    @Update("update user set username = #{username}, email = #{email} where id = #{id}")
    int update(User user);

    @Delete("delete from user where id = #{id}")
    int deleteById(Long id);
}
```

#### **二、多参数、RowBounds、ResultHandler、@Param、@MapKey**

```java
public interface UserMapper {

    @Select("select id, username, email from user where username = #{username} and email = #{email}")
    User selectByUsernameAndEmail(@Param("username") String username,
                                  @Param("email") String email);

    @Select("select id, username, email from user order by id")
    List<User> selectPage(RowBounds rowBounds);

    @Select("select id, username, email from user")
    void scanAll(ResultHandler<User> handler);

    @MapKey("id")
    @Select("select id, username, email from user")
    Map<Long, User> selectToMap();
}
```

#### **三、@Options配置高级属性**

```java
public interface UserMapper {

    @Select("select id, username, email from user where id = #{id}")
    @Options(useCache = true,
             flushCache = Options.FlushCachePolicy.FALSE,
             timeout = 3000,
             fetchSize = 100,
             resultSetType = ResultSetType.SCROLL_SENSITIVE,
             statementType = StatementType.PREPARED)
    User selectWithOptions(Long id);

    @Update("update user set email = #{email} where id = #{id}")
    @Options(flushCache = Options.FlushCachePolicy.TRUE,
             useCache = false,
             timeout = 5000)
    int updateEmail(@Param("id") Long id, @Param("email") String email);
}
```

#### **四：主键生成：@Options(useGeneratedKeys)与@SelectKey**

使用JDBC自动生成主键

```java
public interface UserMapper {

    @Insert("insert into user(username, email) values(#{username}, #{email})")
    @Options(useGeneratedKeys = true, keyProperty = "id", keyColumn = "id")
    int insertAndUseGeneratedKey(User user);
}
```

使用@SelectKey自定义获取主键

```java
public interface UserMapper {

    @Insert("insert into user(id, username, email) values(#{id}, #{username}, #{email})")
    @SelectKey(statement = "select seq_user.nextval from dual",
               keyProperty = "id",
               before = true,
               resultType = Long.class)
    int insertWithSelectKey(User user);
}
```

#### 五：结果映射：@Results/@Result与@ResultMap

直接使用@Results/@Result

```java
public interface UserMapper {

    @Select("select id, username, email from user where id = #{id}")
    @Results(id = "userResultMap", value = {
        @Result(id = true, column = "id", property = "id"),
        @Result(column = "username", property = "username"),
        @Result(column = "email", property = "email")
    })
    User selectWithResults(Long id);

    @Select("select id, username, email from user")
    @ResultMap("userResultMap")
    List<User> selectAllWithResultMap();
}
```

@Result 搭配嵌套查询（一对一/一对多）

```java
public class Order {
    private Long id;
    private Long userId;
    private String orderNo;
}

public interface OrderMapper {

    @Select("select id, user_id, order_no from orders where user_id = #{userId}")
    List<Order> selectByUserId(Long userId);
}
```

```java
public interface UserMapper {

    @Select("select id, username, email from user where id = #{id}")
    @Results(id = "userWithOrdersMap", value = {
        @Result(id = true, column = "id", property = "id"),
        @Result(column = "username", property = "username"),
        @Result(column = "email", property = "email"),
        @Result(property = "orders",
                column = "id",
                many = @Many(select = "org.example.mapper.OrderMapper.selectByUserId",
                             fetchType = FetchType.LAZY))
    })
    User selectUserWithOrders(Long id);
}
```

#### 六：构造函数映射：@ConstructorArgs/@Arg

```java
public class User {
    private final Long id;
    private final String username;
    private final String email;

    public User(Long id, String username, String email) {
        this.id = id;
        this.username = username;
        this.email = email;
    }
}
```

```java
public interface UserMapper {

    @Select("select id, username, email from user where id = #{id}")
    @ConstructorArgs({
        @Arg(id = true, column = "id", javaType = Long.class),
        @Arg(column = "username", javaType = String.class),
        @Arg(column = "email", javaType = String.class)
    })
    User selectWithConstructorArgs(Long id);
}
```

#### 七：结果多态：@TypeDiscriminator/@Case

```

```

