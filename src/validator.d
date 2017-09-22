import std.format;
import std.algorithm;
import std.stdio;

import ast;

void completeFunction(Module mod, FunctionSignature func)
{
    func.returnType = completeType(mod, func.returnType);

    foreach (param; func.parameters)
    {
        param.type = completeType(mod, param.type);
    }

    // Inject 'this' parameter
    auto method = cast(MethodSignature)func;
    if (method !is null)
    {
        auto thisType = method.containerType.clone();
        thisType.pointerDepth++;
        auto thisParam = new TypeSignature(thisType, "this");
        func.parameters = thisParam ~ func.parameters;
    }
}

class ValidationResult
{
    // Inject these statements before the statement being validated
    StatementBase[] injectBefore;

    // Inject these statements after the statement being validated
    StatementBase[] injectAfter;

    // Remove the thing being validated
    bool remove;
}

void validate(Module mod)
{
    // Complete struct definitions
    foreach (struct_; mod.structs)
    {
        validateStruct(mod, struct_);
    }

    // Complete globals
    foreach (global; mod.globals)
    {
        global.signature.type = completeType(mod, global.signature.type);
    }

    // Complete function signatures
    foreach (func; mod.functionsAndMethods())
    {
        completeFunction(mod, func.signature);
    }

    // Complete functions
    foreach (func; mod.functionsAndMethods())
    {
        validateFunction(mod, func);
    }
}

void validateStruct(Module mod, Struct struct_)
{
    foreach (member; struct_.members)
    {
        member.type = completeType(mod, member.type);
    }
}

void validateFunction(Module mod, Function func)
{
    // Complete function body
    validateStatement(mod, func.block);

    // Add default return statement if missing in void function
    if (!func.signature.returnType.compare(new VoidType()))
    {
        return;
    }

    if (func.block.statements.length == 0)
    {
        return;
    }

    if (cast(Return)func.block.statements[$-1] is null)
    {
        auto ret = new Return(null, null);
        ret.parent = func.block;
        validateStatement(mod, ret);
        func.block.statements ~= ret;
    }
}

StatementBase[] validateCleanup(Module mod, StatementBase parent,
                                StatementBase[] previous)
{
    StatementBase[] ret;

    foreach (prev; previous)
    {
        auto defer = cast(Defer)prev;
        if (defer !is null)
        {
            // TODO: handle result of this?
            validateStatement(mod, defer.statement);
            defer.statement.parent = parent;
            ret ~= defer.statement;

            continue;
        }

        auto localDecl = cast(LocalDeclaration)prev;
        if (localDecl !is null)
        {
            auto structType = cast(StructType)localDecl.signature.type;

            if (structType is null)
            {
                continue;
            }

            // Find a destruct method if one exists
            MethodSignature destructMethod = null;

            foreach (met; structType.struct_.methods)
            {
                auto sig = met.methodSignature;

                auto voidRet = cast(VoidType)sig.returnType;
                if (voidRet is null)
                {
                    continue;
                }

                // TODO: we should still know at this point if a function is
                // called template - mangling should happen later, probably in
                // the generator.
                if (!sig.name.canFind("destruct"))
                {
                    continue;
                }

                if (sig.parameters.length != 1 || sig.variadic)
                {
                    continue;
                }

                destructMethod = sig;
                break;
            }

            if (destructMethod is null)
            {
                continue;
            }

            auto sig = new TypeSignature(structType,
                    localDecl.signature.name);
            auto binding = new Binding(sig, sig.name);
            auto call = new MethodCall(binding, "destruct", []);
            auto sta = new Statement(null, call);

            sta.parent = parent;
            validateStatement(mod, sta);
            sta.parent = parent;
            ret ~= sta;

            continue;
        }
    }

    return ret;
}

ValidationResult validateStatement(Module mod, StatementBase st)
{
    auto ret = new ValidationResult();

    // Break out a constructor call if present
    MethodCall constructorCall = null;
    auto local = cast(LocalDeclaration)st;
    if (local !is null)
    {
        constructorCall = cast(MethodCall)local.value;

        if (constructorCall !is null &&
            constructorCall.functionName == "construct")
        {
            local.value = null;
        }
        else
        {
            constructorCall = null;
        }
    }

    foreach (childNode; st.childNodes)
    {
        validateNode(mod, childNode);
    }

    if (local !is null)
    {
        local.signature.type = completeType(mod, local.signature.type);

        if (local.signature.type.isPrimitive(Primitive.Auto))
        {
            if (local.value is null)
            {
                throw new Exception(format("Can't infer type for %s",
                        local.signature));
            }

            local.signature.type = local.value.type;
        }
    }

    // Insert the constructor call (if it exists) after the declaration
    // TODO: use a tag or something instead of string checking?
    if (constructorCall !is null)
    {
        auto constructorSt = new Statement(local.line, constructorCall);

        auto binding = cast(Binding)constructorCall.container;
        if (binding is null)
        {
            throw new Exception("Expected a struct");
        }

        binding.local = local.signature;

        validateStatement(mod, constructorSt);

        ret.injectAfter ~= constructorSt;
    }

    auto retSt = cast(Return)st;
    if (retSt !is null)
    {
        // Generate an unused variable name to store the temporary return value
        // TODO: pretty gross; put these in a totally different namespace or
        // maybe have the generator phase work this out?

        // Save returned expression in a local
        TypeSignature sig;
        if (retSt.expression !is null)
        {
            auto name = format("__ret%d",
                    retSt.containingFunction().nextGeneratedIndex++);
            auto type = retSt.containingFunction().returnType;
            sig = new TypeSignature(type, name);
            auto retVar = new LocalDeclaration(retSt.line, sig,
                    retSt.expression);
            retVar.parent = retSt.parent;
            validateStatement(mod, retVar);
            ret.injectBefore ~= retVar;
        }

        // Process things like destruct calls
        ret.injectBefore ~= validateCleanup(mod, st,
                previousStatementsAll(mod, st));

        // Return the local
        if (retSt.expression !is null)
        {
            retSt.expression = new Binding(sig, sig.name);
            retSt.expression.parent = retSt;
        }
    }

    auto block = cast(Block)st;
    if (block is null)
    {
        foreach (childStatement; st.childStatements)
        {
            // TODO: handle result?
            validateStatement(mod, childStatement);
        }
    }
    else
    {
        StatementBase[] newChildren;

        foreach (childStatement; block.statements)
        {
            auto res = validateStatement(mod, childStatement);

            newChildren ~= res.injectBefore;

            if (!res.remove)
            {
                newChildren ~= childStatement;
            }

            newChildren ~= res.injectAfter;
        }

        block.statements = newChildren;

        if (block.statements.length > 0)
        {
            auto lastStatement = block.statements[$-1];
            auto blockReturn = cast(Return)lastStatement;
            if (blockReturn is null)
            {
                StatementBase[] before = previousStatements(
                        mod, lastStatement);

                before = lastStatement ~ before;

                block.statements ~= validateCleanup(mod, block, before);
            }
        }
    }

    return ret;
}

Type completeType(Module mod, Type type)
{
    if (type is null)
    {
        return null;
    }

    // Already completed, do nothing
    auto incomplete = cast(IncompleteType)type;
    if (incomplete is null)
    {
        return type;
    }

    // Complete the type
    Type ret = null;

    // Try void
    if (incomplete.name == "void")
    {
        return new VoidType();
    }

    // Try primitives
    if (ret is null)
    {
        try
        {
            ret = new PrimitiveType(parsePrimitive(incomplete.name));
        }
        catch (Throwable)
        {
        }
    }

    // Try struct templates
    if (ret is null)
    {
        StructTemplate st = null;

        try
        {
            st = mod.findStructTemplate(incomplete.moduleName,
                    incomplete.name);
        }
        catch (NotFoundException)
        {
        }

        if (st !is null)
        {
            if (st.typeParameters.length != incomplete.typeParameters.length)
            {
                throw new Exception("Mismatched template parameters");
            }

            // Complete type parameters
            for (int i = 0; i < incomplete.typeParameters.length; i++)
            {
                incomplete.typeParameters[i] = completeType(mod,
                        incomplete.typeParameters[i]);
            }

            StructRendering rendering = null;

            // Check existing renderings
            renderingLoop: foreach (r; st.renderings)
            {
                // Match type parameters
                for (int i = 0; i < r.typeParameters.length; i++)
                {
                    auto ct = incomplete.typeParameters[i];
                    auto rt = r.typeParameters[i];

                    if (!incomplete.typeParameters[i].compare(
                            r.typeParameters[i]))
                    {
                        continue renderingLoop;
                    }
                }

                // Found a matching rendering
                rendering = r;
            }

            if (rendering is null)
            {
                // Make a new rendering for this template
                rendering = st.render(incomplete.typeParameters);
                validateStruct(mod, rendering.rendering);

                // Render destruct if it exists
                Method destructRendering = null;

                foreach (method; rendering.structTemplate.methods)
                {
                    if (method.signature.name == "destruct")
                    {
                        destructRendering = rendering.renderMethod(method);
                        break;
                    }
                }

                if (destructRendering !is null)
                {
                    completeFunction(mod, destructRendering.signature);
                    validateFunction(mod, destructRendering);
                    rendering.rendering.methods ~= destructRendering;
                }
            }

            ret = new StructType(rendering.rendering);
        }
    }

    // Try structs from this module
    if (ret is null && incomplete.moduleName is null)
    {
        foreach (s; mod.structs)
        {
            if (s.name == incomplete.name)
            {
                ret = new StructType(s);
                break;
            }
        }
    }

    // Try imported unqualified structs
    if (ret is null && incomplete.moduleName is null)
    {
        importLoop: foreach (imp; mod.imports)
        {
            foreach (s; imp.structs)
            {
                if (s.name == incomplete.name)
                {
                    ret = new StructType(s);
                    break importLoop;
                }
            }
        }
    }

    // Try imported qualified structs
    if (ret is null && incomplete.moduleName !is null)
    {
        auto imp = mod.findImport(incomplete.moduleName);

        foreach (s; imp.structs)
        {
            if (s.name == incomplete.name)
            {
                ret = new StructType(s);
                break;
            }
        }
    }

    if (ret is null)
    {
        throw new Exception(format("Can't complete type %s", type));
    }

    ret.pointerDepth = incomplete.pointerDepth;
    ret.elements = incomplete.elements;

    return ret;
}

void validateMethodCall(Module mod, MethodCall call)
{
    auto structType = cast(StructType)call.container.type;
    if (structType is null)
    {
        // TODO: enforce this with the type system
        throw new Exception("Method container must be a struct");
    }

    auto struct_ = structType.struct_;

    // If this is a struct template method, render the needed method if it
    // hasn't been rendered already.
    auto r = mod.findStructRendering(structType.struct_);
    if (r !is null)
    {
        struct_ = r.rendering;

        // TODO: shouldn't need to render the method before checking if it
        // needed to be rendered
        Method newRendering = null;

        foreach (method; r.structTemplate.methods)
        {
            if (method.signature.name == call.functionName)
            {
                newRendering = r.renderMethod(method);
                break;
            }
        }

        if (newRendering is null)
        {
            throw new Exception("Method template not found");
        }

        // See if the method rendering already exists
        foreach (method; r.rendering.methods)
        {
            if (method.signature.name == newRendering.signature.name)
            {
                call.methodSignature = method.methodSignature;
                break;
            }
        }

        // If the method rendering doesn't already exist, add it
        if (call.methodSignature is null)
        {
            completeFunction(mod, newRendering.signature);
            validateFunction(mod, newRendering);
            call.methodSignature = newRendering.methodSignature;
            r.rendering.methods ~= newRendering;
        }
    }

    if (call.methodSignature is null)
    {
        call.methodSignature = struct_.findMethod(call);
    }

    call.retype();

    // Inject 'this' argument
    // TODO: shouldn't need to guard this, it shouldn't be called multiple
    // times
    if (call.parameters.length == 0 || call.parameters[0].tag != "this")
    {
        auto thisArg = new Reference(call.container);
        thisArg.tag = "this";
        thisArg.retype();
        call.parameters = thisArg ~ call.parameters;
        thisArg.parent = call;
    }
}

// TODO: this seems to be getting called multiple times for the same call
void validateCall(Module mod, Call call)
{
    // Search function templates
    FunctionTemplate[] functionTemplates;

    // No module scope: search current module and unqualified imports
    if (call.moduleName is null)
    {
        functionTemplates = mod.unqualifiedFunctionTemplates();
    }
    // Module scope: search the named module only
    else
    {
        functionTemplates = mod.findImport(call.moduleName).functionTemplates;
    }

    templateLoop: foreach (ft; functionTemplates)
    {
        if (ft.signature.name != call.functionName)
            continue;

        if (ft.typeParameters.length != call.typeParameters.length)
            throw new Exception("Mismatched template parameters");

        for (int i = 0; i < call.typeParameters.length; i++)
        {
            call.typeParameters[i] = completeType(mod, call.typeParameters[i]);
        }

        // Check existing renderings
        renderingLoop: foreach (r; ft.renderings)
        {
            // Match type parameters
            for (int i = 0; i < r.typeParameters.length; i++)
            {
                auto ct = call.typeParameters[i];
                auto rt = r.typeParameters[i];

                if (!call.typeParameters[i].compare(r.typeParameters[i]))
                {
                    continue renderingLoop;
                }
            }

            // Found a matching rendering
            call.targetSignature = r.rendering.signature;
            break templateLoop;
        }

        // Make a new rendering for this template
        auto newRendering = ft.render(call.typeParameters);
        validateFunction(mod, newRendering.rendering);
        ft.renderings ~= newRendering;
        call.targetSignature = newRendering.rendering.signature;

        break;
    }

    // No template found? Normal function lookup
    if (call.targetSignature is null)
    {
        call.targetSignature = mod.findFunction(call);
    }

    call.retype();
}

void validateNode(Module mod, Node node)
{
    foreach (childNode; node.childNodes())
    {
        validateNode(mod, childNode);
    }

    auto methodCall = cast(MethodCall)node;
    if (methodCall !is null)
    {
        validateMethodCall(mod, methodCall);
        return;
    }

    auto call = cast(Call)node;
    if (call !is null)
    {
        validateCall(mod, call);
        return;
    }

    auto binding = cast(Binding)node;
    if (binding !is null)
    {
        if (binding.local is null)
        {
            binding.local = findLocal(mod, binding, binding.name);
        }
        node.retype();
        return;
    }

    auto sizeof = cast(SizeOf)node;
    if (sizeof !is null)
    {
        sizeof.argument = completeType(mod, sizeof.argument);
        node.retype();
        return;
    }

    node.type = completeType(mod, node.type);
    node.retype();
}

// Find the statement is part of.
StatementBase parentStatement(Module mod, Node node)
{
    // Walk up the tree until the first statement is found
    while (node.parent !is null)
    {
        auto parentNode = cast(Node)node.parent;
        if (parentNode !is null)
        {
            node = parentNode;
            continue;
        }

        auto statement = cast(StatementBase)node.parent;
        if (statement !is null)
        {
            return statement;
        }
    }

    throw new Exception(format("Found an orphaned node: %s", node));
}

StatementBase nextWithBlockParent(StatementBase st)
{
    while (cast(Block)st.parent is null)
    {
        auto parentStatement = cast(StatementBase)st.parent;
        if (parentStatement !is null)
        {
            st = parentStatement;
            continue;
        }

        return null;
    }

    return st;
}

// Find all statements that came before the given statement in its block.
StatementBase[] previousStatements(Module mod, StatementBase statement)
{
    StatementBase[] ret;

    // Find the nearest Block ancestor
    statement = nextWithBlockParent(statement);
    if (statement is null)
    {
        throw new Exception("No more parents");
    }

    auto parent = cast(Block)statement.parent;

    // Add sibling statements that come before this statement in the block
    foreach (st; parent.statements)
    {
        if (st == statement)
        {
            break;
        }

        ret = st ~ ret;
    }

    return ret;
}

// Find all statements that came before the given statement, walking up the
// tree to show containing blocks as well.
StatementBase[] previousStatementsAll(Module mod, StatementBase statement)
{
    // Find the nearest Block ancestor
    statement = nextWithBlockParent(statement);
    if (statement is null)
    {
        throw new Exception("No more parents");
    }

    auto parent = cast(Block)statement.parent;

    auto ret = previousStatements(mod, statement);

    // Continue walking up the tree
    auto parentStatement = cast(StatementBase)parent.parent;
    if (parentStatement !is null)
    {
        ret ~= previousStatementsAll(mod, parentStatement);
    }

    return ret;
}

TypeSignature findLocal(Module mod, Node node, string name)
{
    return findLocal(mod, parentStatement(mod, node), name);
}

TypeSignature findLocal(Module mod, StatementBase statement, string name)
{
    foreach (st; previousStatementsAll(mod, statement))
    {
        auto local = cast(LocalDeclaration)st;
        if (local !is null)
        {
            if (local.signature.name == name)
            {
                return local.signature;
            }
        }
    }

    // Local not found in this function: check the function/method parameters
    auto func = statement.containingFunction();
    auto parameters = func.parameters;
    foreach (param; parameters)
    {
        if (param.name == name)
        {
            return param;
        }
    }

    // Last resort: check the globals
    foreach (global; mod.globals)
    {
        if (global.signature.name == name)
        {
            return global.signature;
        }
    }

    throw new Exception(format("Local %s not in scope", name));
}
