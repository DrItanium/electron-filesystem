package clips;

public class StringValue extends PrimitiveValue
{
	/****************/
	/* StringValue: */
	/****************/
	public StringValue()
	{
		super(new String(""));
	}

	/****************/
	/* StringValue: */
	/****************/
	public StringValue(
			String value)
	{
		super(value);
	}

	/****************/
	/* lexemeValue: */
	/****************/
	public String lexemeValue() throws Exception
	{
		return (String) getValue();
	}

	/****************/
	/* stringValue: */
	/****************/
	public String stringValue() throws Exception
	{
		return (String) getValue();
	}

	/************/
	/* retain: */
	/************/
	public void retain()
	{
		//System.out.println("StringValue retain");
	}

	/************/
	/* release: */
	/************/
	public void release()
	{
		//System.out.println("StringValue release");
	}

	/*************/
	/* toString: */
	/*************/
	public String toString()
	{        
		return "\"" + super.toString() + "\"";
	}

}
