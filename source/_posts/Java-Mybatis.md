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

## Mybatis

### I：前言

版本：`<mybatis-plus.version>3.4.1</mybatis-plus.version>`

定位：协助完全控制SQL，抽取封装模板代码。

分析原则：度量取舍

### II：分析

#### A-00X：全局配置文件解析

只分析从xml配置文件解析，其它路径殊途同归，暂不分析

**SqlSessionFactoryBuilder**

```java
String resource = "org/apache/ibatis/test/mybatis.xml";
InputStream inputStream = Resources.getResourceAsStream(resource);
SqlSessionFactory sqlSessionFactory = new SqlSessionFactoryBuilder().build(inputStream);
```

**XMLConfigBuilder**

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

**MapperAnnotationBuilder**

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

**MapperBuilderAssistant**

### III：使用

#### A-00X：Mybatis注解开发

**实体类**

```java
public class User {
  private Long id;
  private String username;
  private String email;
}
```

**一、基础CRUD注解**

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

**二、多参数、RowBounds、ResultHandler、@Param、@MapKey**

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

**三、@Options配置高级属性**

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

**四：主键生成：@Options(useGeneratedKeys)与@SelectKey**

```java
public interface UserMapper {

  @Insert("insert into user(username, email) values(#{username}, #{email})")
  @Options(useGeneratedKeys = true, keyProperty = "id", keyColumn = "id")
  int insertAndUseGeneratedKey(User user);
}
```

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

