package asdl_base;
public final class identifier_list {
    public identifier head;
    public identifier_list tail;
    
    public identifier_list(identifier head, identifier_list tail)
    {
        this.head = head;
        this.tail = tail;
    }
    
}
